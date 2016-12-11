module Game.Cubbit.PointerLock (requestPointerLock, exitPointerLock) where

import Control.Monad.Eff (Eff)
import DOM (DOM)
import Data.Unit (Unit)

foreign import requestPointerLock :: ∀eff . ({ movementX :: Number, movementY :: Number } -> Eff (dom :: DOM | eff) Unit) -> Eff (dom :: DOM | eff) Unit -> Eff (dom :: DOM | eff) Unit

foreign import exitPointerLock :: ∀eff . Eff (dom :: DOM | eff) Unit