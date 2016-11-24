module Main (main) where

import Control.Monad.Eff (Eff)
import Data.Unit (Unit)
import Game.Cubbit.Main (main) as Cubbit
import Game.Cubbit.Types (Effects)

main :: forall eff. Eff (Effects eff) Unit
main = Cubbit.main

