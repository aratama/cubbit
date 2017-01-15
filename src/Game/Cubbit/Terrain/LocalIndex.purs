module Game.Cubbit.LocalIndex (LocalIndex, localIndex, runLocalIndex) where

import Control.Alternative (pure)
import Data.Foreign (toForeign, unsafeFromForeign)
import Data.Foreign.Class (class AsForeign, class IsForeign)
import Data.Generic (class Generic, gCompare, gEq)
import Data.Ord (class Ord)
import Data.Show (show)
import Game.Cubbit.Constants (chunkSize)
import Prelude (class Eq, class Show, (+), (-), (*), (/), mod)
import Data.Newtype (class Newtype)

newtype LocalIndex = LocalIndex Int

derive instance newtype_LocalIndex :: Newtype LocalIndex _

localIndex :: Int -> Int -> Int -> LocalIndex
localIndex lx ly lz = LocalIndex (chunkSize * chunkSize * lx + chunkSize * ly + lz)

runLocalIndex :: LocalIndex -> { x :: Int, y :: Int, z :: Int }
runLocalIndex (LocalIndex i) = { x, y, z }
  where
    x = i / (chunkSize * chunkSize)
    i' = i - x * chunkSize * chunkSize
    y = i' / chunkSize
    z = mod i'  chunkSize

derive instance generic_LocalIndex :: Generic LocalIndex

instance eq_LocalIndex :: Eq LocalIndex where
    eq = gEq

instance ord_LocalIndex :: Ord LocalIndex where
    compare = gCompare

instance show_LocalIndex :: Show LocalIndex where
    show (LocalIndex i) = show i

instance isForeign_LocalIndex :: IsForeign LocalIndex where
    read value = pure (unsafeFromForeign value)

instance asForeign_LocalIndex :: AsForeign LocalIndex where
    write = toForeign


