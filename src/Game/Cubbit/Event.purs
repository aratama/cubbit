module Game.Cubbit.Event where

import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (Error)
import DOM (DOM)
import Data.Unit (Unit)

foreign import onButtonClick :: forall eff. String -> Eff (dom :: DOM | eff) Unit -> Eff (dom :: DOM | eff) Unit

foreign import onMouseMove :: forall eff. ({ offsetX :: Int, offsetY :: Int } -> Eff (dom :: DOM | eff) Unit) -> Eff (dom :: DOM | eff) Unit

foreign import onMouseClick :: forall eff. ({ offsetX :: Int, offsetY :: Int } -> Eff (dom :: DOM | eff) Unit) -> Eff (dom :: DOM | eff) Unit

wait :: forall eff. Aff (dom :: DOM | eff) Unit
wait = makeAff \reject resolve -> _wait reject resolve

foreign import _wait :: forall eff. (Error -> Eff (dom :: DOM | eff) Unit) -> (Unit -> Eff (dom :: DOM | eff) Unit) -> Eff (dom :: DOM | eff) Unit

foreign import onKeyDown :: forall eff. ({ keyCode :: Int } -> Eff (dom :: DOM | eff) Unit) -> Eff (dom :: DOM | eff) Unit

foreign import onKeyUp :: forall eff. ({ keyCode :: Int } -> Eff (dom :: DOM | eff) Unit) -> Eff (dom :: DOM | eff) Unit

foreign import focus :: forall eff. String -> Eff (dom :: DOM | eff) Unit