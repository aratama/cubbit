module Game.Cubbit.Hud.Terrain (initializeTerrain) where

import Control.Bind (join, when)
import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.RWS (modify)
import Control.Monad.Rec.Class (Step(Loop, Done), tailRecM2)
import Control.MonadPlus (guard)
import DOM.Event.EventTarget (addEventListener, eventListener)
import DOM.HTML (window)
import DOM.HTML.Event.EventTypes (resize) as DOM
import DOM.HTML.HTMLElement (focus)
import DOM.HTML.Types (htmlDocumentToNonElementParentNode, windowToEventTarget)
import DOM.HTML.Window (document)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (ElementId(..))
import Data.Array (findIndex, length, (!!), (..))
import Data.BooleanAlgebra (not)
import Data.EuclideanRing (mod)
import Data.Maybe (Maybe(..))
import Data.Nullable (toMaybe)
import Data.Traversable (for_, traverse_)
import Data.Unit (Unit, unit)
import Data.Void (Void)
import Game.Cubbit.ChunkIndex (chunkIndex)
import Game.Cubbit.Collesion (buildCollesionTerrain, createPlayerCollesion, updatePhysics)
import Game.Cubbit.Hud.Start (clearTerrain)
import Game.Cubbit.Hud.Type (HudEffects, Query)
import Game.Cubbit.MeshBuilder (generateChunk)
import Game.Cubbit.Option (Options(Options))
import Game.Cubbit.Resources (Resources, loadResourcesH)
import Game.Cubbit.Terrain (Terrain(Terrain), createTerrain)
import Game.Cubbit.Types (GameMode(MultiplayerMode, SinglePlayerMode), SceneState(ModeSelectionSceneState, TitleSceneState, LoadingSceneState), State(State))
import Game.Cubbit.Update (updateBabylon, updateH, updateSound)
import Gamepad (Gamepad(..), GamepadButton(..), getGamepads, onButtonPress)
import Graphics.Babylon.Engine (getDeltaTime, resize, runRenderLoop)
import Graphics.Babylon.Scene (render)
import Graphics.Babylon.Util (querySelectorCanvasAff)
import Graphics.Cannon (addBody, setGravity)
import Graphics.Cannon.Vec3 (createVec3)
import Halogen (ComponentDSL, liftEff, put)
import Halogen.Query (get, lift)
import Prelude (bind, negate, pure, ($), (&&), (+), (-), (<#>), (<$>), (<<<), (==), (>>=))
import Unsafe.Coerce (unsafeCoerce)

initializeTerrain :: forall eff. Resources -> ComponentDSL State Query Void (Aff (HudEffects eff)) Unit
initializeTerrain res@{ options: Options options } = do
    State state@{ terrain: Terrain terrain } <- get

    liftEff $ clearTerrain (Terrain terrain) state.world

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
            ter' <- liftEff $ buildCollesionTerrain ter state.world (chunkIndex 0 0 0)
            pure $ Loop { a: ter', b: i - 1 }
    ) emptyTerrain 9

    modify $ \(State s) -> State s { terrain = terrain' }
