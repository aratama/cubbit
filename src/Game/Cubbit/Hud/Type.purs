module Game.Cubbit.Hud.Type (Query(..), QueryA(..), HudEffects, PlayingSceneQuery(..), HudDriver) where

import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Timer (TIMER)
import DOM.Event.Event (Event)
import DOM.Event.KeyboardEvent (KeyboardEvent)
import DOM.Event.MouseEvent (MouseEvent)
import DOM.WebStorage (STORAGE)
import Data.Void (Void)
import Game.Cubbit.BlockIndex (BlockIndex)
import Game.Cubbit.Types (Mode, State)
import Game.Cubbit.Vec (Vec)
import Graphics.Babylon.Types (BABYLON)
import Halogen (HalogenEffects, HalogenIO)
import Network.HTTP.Affjax (AJAX)

data Query a = Query QueryA a

data QueryA = PlayingSceneQuery PlayingSceneQuery
             | PreventDefault Event
             | Nop Event
             | ToggleMute
             | Start
             | ShowConfig
             | CloseConfig
             | SetBGMVolume Int
             | SetSEVolume Int
             | StopPropagation Event
             | ToggleShadow
             | ToggleVertexColor
             | SetShadowArea Int
             | SetChunkArea Int
             | Repaint State

data PlayingSceneQuery = SetCursorPosition BlockIndex
                         | SetMode Mode
                         | SetPosition Vec
                         | TogglePointerLock
                         | SetMousePosition MouseEvent
                         | OnMouseClick MouseEvent
                         | Zoom MouseEvent
                         | OnKeyDown KeyboardEvent
                         | OnKeyUp KeyboardEvent
                         | SetCenterPanelVisible Boolean
                         | Home

type HudEffects eff = HalogenEffects (
    console :: CONSOLE,
    ajax :: AJAX,
    babylon :: BABYLON,
    storage :: STORAGE,
    timer :: TIMER | eff)

type HudDriver eff = HalogenIO Query Void (Aff (HudEffects eff))
