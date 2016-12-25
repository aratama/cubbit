module Game.Cubbit.BoxelMap (BoxelMap, lookup, insert, delete) where

import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Game.Cubbit.BlockType (BlockType)
import Game.Cubbit.Constants (chunkSize)
import Game.Cubbit.LocalIndex (LocalIndex)
import Prelude ((*))

foreign import data BoxelMap :: *

foreign import lookupNullable :: LocalIndex -> BoxelMap -> Nullable BlockType

lookup :: LocalIndex -> BoxelMap -> Maybe BlockType
lookup key map = toMaybe (lookupNullable key map)

foreign import insert :: LocalIndex -> BlockType -> BoxelMap -> BoxelMap

foreign import delete :: LocalIndex -> BoxelMap -> BoxelMap

foreign import _empty :: Int -> BoxelMap

empty :: BoxelMap
empty = _empty (chunkSize * chunkSize * chunkSize)

