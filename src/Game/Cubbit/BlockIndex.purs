module Game.Cubbit.BlockIndex (BlockIndex, blockIndex, runBlockIndex, eqBlockIndex, showBlockIndex) where

import Control.Alternative (pure)
import Data.Foreign (toForeign, unsafeFromForeign)
import Data.Foreign.Class (class AsForeign, class IsForeign)
import Prelude (class Eq, class Show)

foreign import data BlockIndex :: *

foreign import blockIndex :: Int -> Int -> Int -> BlockIndex

foreign import runBlockIndex :: BlockIndex -> { x :: Int, y :: Int, z :: Int }

foreign import eqBlockIndex :: BlockIndex -> BlockIndex -> Boolean

foreign import showBlockIndex :: BlockIndex -> String

instance is_Foreign_Index3D :: IsForeign BlockIndex where
    read value = pure (unsafeFromForeign value)

instance as_Foreign_Index3D :: AsForeign BlockIndex where
    write = toForeign

instance eq_BlockIndex :: Eq BlockIndex where
    eq = eqBlockIndex

instance show_BlockIndex :: Show BlockIndex where
    show = showBlockIndex

