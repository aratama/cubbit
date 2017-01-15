module Raven where

import Control.Monad.Eff (Eff)
import DOM (DOM)
import Data.Unit (Unit)

foreign import installRaven :: forall eff. String -> Eff (dom :: DOM | eff) Unit


foreign import reportToSentry :: forall eff. Eff (dom :: DOM | eff) Unit