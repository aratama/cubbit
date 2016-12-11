module Game.Cubbit.Hud.Type (Query(..), HudEffects, HudState, GameScene(..)) where

import Control.Monad.Eff.Timer (TIMER)
import DOM.Event.Event (Event)
import DOM.Event.KeyboardEvent (KeyboardEvent)
import DOM.Event.MouseEvent (MouseEvent)
import DOM.WebStorage (STORAGE)
import Data.Maybe (Maybe)
import Game.Cubbit.BlockIndex (BlockIndex)
import Game.Cubbit.Types (Mode)
import Game.Cubbit.Vec (Vec)
import Graphics.Babylon.Types (BABYLON)
import Halogen (HalogenEffects)
import Network.HTTP.Affjax (AJAX)

type HudState = {
    cursorPosition :: BlockIndex,
    mode :: Mode,
    mute :: Boolean,
    centerPanelVisible :: Boolean,
    nextScene :: Maybe GameScene,
    gameScene :: GameScene,
    life :: Int,
    maxLife :: Int
}

data GameScene = TitleScene | PlayingScene

data Query a = SetCursorPosition BlockIndex a
             | PreventDefault Event a
             | SetMode Mode a
             | SetPosition Vec a
             | TogglePointerLock a
             | SetMousePosition MouseEvent a
             | OnMouseClick MouseEvent a
             | ToggleDebugLayer a
             | Zoom MouseEvent a
             | OnKeyDown KeyboardEvent a
             | OnKeyUp KeyboardEvent a
             | ToggleMute a
             | SetCenterPanelVisible Boolean a
             | Nop Event a
             | Start a
             | Home a

type HudEffects eff = HalogenEffects (
    ajax :: AJAX,
    babylon :: BABYLON,
    storage :: STORAGE,
    timer :: TIMER | eff)



