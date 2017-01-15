module Gamepad where

import Control.Monad.Eff (Eff)
import DOM (DOM)
import Data.Array ((!!))
import Data.Eq (class Eq)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe, fromMaybe, maybe)
import Data.Nullable (Nullable, toMaybe)
import Prelude (map, (<$>), (&&), not, bind, join, pure)

newtype GamepadButton = GamepadButton {
    value :: Number,
    pressed :: Boolean
}

derive instance genericGamepadButton :: Generic GamepadButton _

derive instance eqGamepadButton :: Eq GamepadButton

newtype Gamepad = Gamepad {
    id :: String,
    index :: Int,
    mapping :: String,
    connected :: Boolean,
    buttons :: Array GamepadButton,
    axes :: Array Number,
    timeStamp :: Number
}

derive instance genericGamepad :: Generic Gamepad _

derive instance eqGamepad :: Eq Gamepad

foreign import _getGamepads :: forall eff. Eff (dom :: DOM | eff) (Array (Nullable Gamepad))

getGamepads :: forall eff. Eff (dom :: DOM | eff) (Array (Maybe Gamepad))
getGamepads = map toMaybe <$> _getGamepads


isButtonDown :: Int -> Int -> Array (Maybe Gamepad) -> Boolean
isButtonDown gamepadIndex buttonIndex gamepads = fromMaybe false do
    Gamepad gamepad <- join (gamepads !! gamepadIndex)
    GamepadButton button <- gamepad.buttons !! buttonIndex
    pure button.pressed

onButtonPress :: Int -> Int -> Array (Maybe Gamepad) -> Array (Maybe Gamepad) -> Boolean
onButtonPress gamepadIndex buttonIndex previousGamepads gamepads =
    not (isButtonDown gamepadIndex buttonIndex previousGamepads) &&
        isButtonDown gamepadIndex buttonIndex gamepads

