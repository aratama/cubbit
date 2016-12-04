module Game.Cubbit.Event (focus) where

import Control.Monad.Eff (Eff)
import DOM (DOM)
import Data.Unit (Unit)

foreign import focus :: forall eff. String -> Eff (dom :: DOM | eff) Unit