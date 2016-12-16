module Game.Cubbit.Aff (wait) where

import Control.Monad.Aff (makeAff)
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Eff.Timer (TIMER, setTimeout)
import Data.Unit (Unit, unit)
import Prelude (void)

wait :: forall m eff. (MonadAff (timer :: TIMER | eff) m) => Int -> m Unit
wait msecs = liftAff (makeAff \reject resolve -> void (setTimeout msecs (resolve unit)))
