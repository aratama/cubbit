module Game.Cubbit.Hud.ModeSelect (modeSelect) where

import Control.Monad.Aff (Aff)
import Control.Monad.RWS (modify)
import Control.Monad.Rec.Class (tailRecM, tailRecM2)
import DOM.Event.Event (preventDefault, stopPropagation)
import Data.Array ((..))
import Data.BooleanAlgebra (not)
import Data.Maybe (Maybe(..))
import Data.Traversable (for_)
import Data.Unit (Unit, unit)
import Data.Void (Void)
import Game.Cubbit.Aff (wait)
import Game.Cubbit.ChunkIndex (chunkIndex)
import Game.Cubbit.Config (Config(Config), writeConfig)
import Game.Cubbit.Hud.EvalPlayingSceneQuery (evalPlayingSceneQuery)
import Game.Cubbit.Hud.Start (clearTerrain, start)
import Game.Cubbit.Hud.Type (HudEffects, Query(Query), QueryA(PlayingSceneQuery, ToggleMute, Start, ModeSelect, Home, SetChunkArea, SetShadowArea, ToggleWaterMaterial, ToggleVertexColor, ToggleShadow, SetSEVolume, SetBGMVolume, CloseConfig, ShowConfig, StopPropagation, Nop, PreventDefault, SetLanguage, SetActiveGameMode, Repaint, Gameloop), getRes)
import Game.Cubbit.MeshBuilder (generateChunk)
import Game.Cubbit.Option (Options(..))
import Game.Cubbit.Resources (Resources)
import Game.Cubbit.Terrain (Terrain(..), createTerrain)
import Game.Cubbit.Types (GameMode(SinglePlayerMode), SceneState(ModeSelectionSceneState, TitleSceneState), State(State))
import Graphics.Babylon.AbstractMesh (setReceiveShadows, setUseVertexColors)
import Graphics.Babylon.Scene (getMeshes)
import Graphics.Babylon.Sound (play)
import Halogen (ComponentDSL, liftEff)
import Halogen.Query (get, put)
import Prelude (bind, pure, ($), negate)
import Game.Cubbit.Hud.Terrain (initializeTerrain)

modeSelect :: forall eff. Resources -> ComponentDSL State Query Void (Aff (HudEffects eff)) Unit
modeSelect res = do
    liftEff $ play res.sounds.warpSound
    modify \(State state) -> State state {
        nextScene = true
    }
    wait 1000
    initializeTerrain res
    modify \(State state) -> State state {
        sceneState = ModeSelectionSceneState { res, mode: SinglePlayerMode },
        nextBGM = Just res.sounds.ichigo
    }
    wait 1000
    modify \(State state) -> State state {
        nextScene = false
    }
    pure unit



