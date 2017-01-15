module PointerLock (addPointerlockchangeListener, requestPointerLock, exitPointerLock, addPointerMoveListener) where

import Control.Monad.Eff (Eff)
import DOM (DOM)
import DOM.HTML.Types (HTMLElement)
import Data.Nullable (Nullable)
import Data.Unit (Unit)

foreign import addPointerlockchangeListener :: forall eff. (Nullable HTMLElement -> Eff (dom :: DOM | eff) Unit) -> Eff (dom :: DOM | eff) Unit

foreign import addPointerMoveListener :: forall eff. ({ movementX :: Number, movementY :: Number } -> Eff (dom :: DOM | eff) Unit) -> Eff (dom :: DOM | eff) Unit

foreign import requestPointerLock :: ∀eff . Eff (dom :: DOM | eff) Unit

foreign import exitPointerLock :: ∀eff . Eff (dom :: DOM | eff) Unit
