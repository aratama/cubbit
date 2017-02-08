module Game.Cubbit.Hud.Terrain (initializeTerrain) where

import Control.Monad.Aff (Aff)
import Control.Monad.RWS (modify)
import Control.Monad.Rec.Class (Step(Loop, Done), tailRecM2)
import Data.Array ((..))
import Data.Traversable (for_)
import Data.Unit (Unit)
import Data.Void (Void)
import Game.Cubbit.ChunkIndex (chunkIndex)
import Game.Cubbit.Collesion (buildCollesionTerrain)
import Game.Cubbit.Hud.Start (clearTerrain)
import Game.Cubbit.Hud.Type (HudEffects, Query)
import Game.Cubbit.MeshBuilder (generateChunk)
import Game.Cubbit.Option (Options(Options))
import Game.Cubbit.Resources (Resources)
import Game.Cubbit.Terrain (Terrain(Terrain), createTerrain)
import Game.Cubbit.Types (State(State))
import Halogen (ComponentDSL, liftEff, put)
import Halogen.Query (get)
import Prelude (bind, negate, pure, ($), (-))

initializeTerrain :: forall eff. Resources -> ComponentDSL State Query Void (Aff (HudEffects eff)) Unit
initializeTerrain res@{ options: Options options } = do
    State state@{ terrain: Terrain terrain } <- get

    liftEff $ clearTerrain (Terrain terrain) res.world

    -- update state
    emptyTerrain <- liftEff $ createTerrain 0
    put $ State state {
        terrain = emptyTerrain
    }

        -- initialize chunk and mesh
    let initialWorldSize = options.initialWorldSize
    for_ ((-initialWorldSize) .. initialWorldSize) \x -> do
        for_ ((-initialWorldSize) .. initialWorldSize) \z -> do
            let index = chunkIndex x 0 z
            State s <- get
            liftEff $ generateChunk (State s) res.materials res.scene index res.options s.config res

    -- initialize cannon world
    terrain' <- tailRecM2 (\ter -> case _ of
        0 -> pure $ Done ter
        i -> do
            ter' <- liftEff $ buildCollesionTerrain ter res.world (chunkIndex 0 0 0)
            pure $ Loop { a: ter', b: i - 1 }
    ) emptyTerrain 9

    modify $ \(State s) -> State s { terrain = terrain' }
