module Game.Cubbit.Hud.Type (Query(..), QueryA(..), HudEffects, PlayingSceneQuery(..), HudDriver, getRes) where

import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Timer (TIMER)
import DOM.Event.Event (Event)
import DOM.Event.KeyboardEvent (KeyboardEvent)
import DOM.Event.MouseEvent (MouseEvent)
import DOM.Event.WheelEvent (WheelEvent)
import DOM.WebStorage (STORAGE)
import Data.Maybe (Maybe(..))
import Data.Void (Void)
import Game.Cubbit.BlockIndex (BlockIndex)
import Game.Cubbit.Captions (Language)
import Game.Cubbit.Resources (Resources)
import Game.Cubbit.Types (GameMode, Mode, State(..), SceneState(..))
import Game.Cubbit.Vec (Vec)
import Graphics.Babylon.Types (BABYLON)
import Graphics.Cannon (CANNON)
import Halogen (HalogenEffects, HalogenIO)
import Network.HTTP.Affjax (AJAX)
import Web.Firebase (FIREBASE)

data Query a = Query QueryA a

data QueryA = PlayingSceneQuery PlayingSceneQuery
             | PreventDefault Event
             | Nop Event
             | ToggleMute
             | ModeSelect Resources
             | Home Resources
             | Start GameMode Resources
             | ShowConfig Resources
             | CloseConfig Resources
             | SetBGMVolume Resources Int
             | SetSEVolume Resources Int
             | StopPropagation Event
             | ToggleShadow Resources
             | ToggleVertexColor Resources
             | SetShadowArea Resources Int
             | SetChunkArea Resources Int
             | ToggleWaterMaterial Resources
             | Repaint State
             | SetLanguage Language Resources

data PlayingSceneQuery = SetCursorPosition BlockIndex
                         | SetMode Mode
                         | SetPosition Vec
                         | TogglePointerLock
                         | SetMousePosition MouseEvent
                         | OnMouseClick MouseEvent
                         | Zoom WheelEvent
                         | OnKeyDown KeyboardEvent
                         | OnKeyUp KeyboardEvent
                         | SetCenterPanelVisible Boolean


type HudEffects eff = HalogenEffects (
    console :: CONSOLE,
    ajax :: AJAX,
    babylon :: BABYLON,
    storage :: STORAGE,
    timer :: TIMER,
    cannon :: CANNON,
    firebase :: FIREBASE | eff)

type HudDriver eff = HalogenIO Query Void (Aff (HudEffects eff))

getRes :: State -> Maybe Resources
getRes (State state) = case state.sceneState of
    TitleSceneState { res } -> Just res
    PlayingSceneState { res } -> Just res
    _ -> Nothing
