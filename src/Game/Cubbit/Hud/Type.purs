module Game.Cubbit.Hud.Type (Query(..), HudEffects, HudState, GameScene(..), PlayingSceneQuery(..)) where

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

    gameScene :: GameScene,

    cursorPosition :: BlockIndex,
    mode :: Mode,
    mute :: Boolean,
    centerPanelVisible :: Boolean,
    nextScene :: Maybe GameScene,
    life :: Int,
    maxLife :: Int
}

data GameScene = TitleScene | PlayingScene

data Query a = PlayingSceneQuery PlayingSceneQuery a
             | PreventDefault Event a
             | Nop Event a
             | ToggleMute a
             | Start a
             | PeekState (HudState -> a)

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



