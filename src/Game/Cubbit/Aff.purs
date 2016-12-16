module Game.Cubbit.Aff (wait, loadImage) where

import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Eff.Timer (TIMER, setTimeout)
import DOM (DOM)
import DOM.HTML.Types (HTMLElement)
import Data.Unit (Unit, unit)
import Prelude (void)

wait :: forall m eff. (MonadAff (timer :: TIMER | eff) m) => Int -> m Unit
wait msecs = liftAff (makeAff \reject resolve -> void (setTimeout msecs (resolve unit)))


loadImage :: forall eff. String -> Aff (dom :: DOM | eff) HTMLElement
loadImage src = makeAff (loadImageEff src)

foreign import loadImageEff :: forall eff. String -> (Error -> Eff (dom :: DOM | eff) Unit) -> (HTMLElement -> Eff (dom :: DOM | eff) Unit) -> Eff (dom :: DOM | eff) Unit
