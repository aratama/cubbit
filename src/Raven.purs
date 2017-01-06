module Raven where

import Control.Monad.Eff (Eff)
import DOM (DOM)
import Data.Unit (Unit)

foreign import install :: forall eff. String -> Eff (dom :: DOM | eff) Unit
