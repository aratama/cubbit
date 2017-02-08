module Cubbit.Gamepad where

import Data.Array ((!!))
import Data.Maybe (Maybe, fromMaybe)
import Prelude (bind, join, not, pure, (&&))
import Gamepad (Gamepad(Gamepad), GamepadButton(GamepadButton))

isButtonDown :: Int -> Int -> Array (Maybe Gamepad) -> Boolean
isButtonDown gamepadIndex buttonIndex gamepads = fromMaybe false do
    Gamepad gamepad <- join (gamepads !! gamepadIndex)
    GamepadButton button <- gamepad.buttons !! buttonIndex
    pure button.pressed

onButtonPress :: Int -> Int -> Array (Maybe Gamepad) -> Array (Maybe Gamepad) -> Boolean
onButtonPress gamepadIndex buttonIndex previousGamepads gamepads =
    not (isButtonDown gamepadIndex buttonIndex previousGamepads) &&
        isButtonDown gamepadIndex buttonIndex gamepads

