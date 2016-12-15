module Game.Cubbit.Hud.Type (Query(..), HudEffects, PlayingSceneQuery(..)) where

import Control.Monad.Eff.Timer (TIMER)
import DOM.Event.Event (Event)
import DOM.Event.KeyboardEvent (KeyboardEvent)
import DOM.Event.MouseEvent (MouseEvent)
import DOM.WebStorage (STORAGE)
import Game.Cubbit.BlockIndex (BlockIndex)
import Game.Cubbit.Types (Mode, State)
import Game.Cubbit.Vec (Vec)
import Graphics.Babylon.Types (BABYLON)
import Halogen (HalogenEffects)
import Network.HTTP.Affjax (AJAX)

data Query a = PlayingSceneQuery PlayingSceneQuery a
             | PreventDefault Event a
             | Nop Event a
             | ToggleMute a
             | Start a
             | PeekState (State -> a)
             | ShowConfig a
             | CloseConfig a

data PlayingSceneQuery = SetCursorPosition BlockIndex
                         | SetMode Mode
                         | SetPosition Vec
                         | TogglePointerLock
                         | SetMousePosition MouseEvent
                         | OnMouseClick MouseEvent
                         | ToggleDebugLayer
                         | Zoom MouseEvent
                         | OnKeyDown KeyboardEvent
                         | OnKeyUp KeyboardEvent
                         | SetCenterPanelVisible Boolean
                         | Home

type HudEffects eff = HalogenEffects (
    ajax :: AJAX,
    babylon :: BABYLON,
    storage :: STORAGE,
    timer :: TIMER | eff)
