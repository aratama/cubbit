module Game.Cubbit.ChunkMap where

import Control.Monad.Eff (Eff)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Data.Unit (Unit)
import Game.Cubbit.ChunkInstance (ChunkInstance)
import Game.Cubbit.ChunkIndex (ChunkIndex)
import Prelude ((<$>))

foreign import data ChunkMap :: *

foreign import createChunkMap :: forall eff. Eff eff ChunkMap

foreign import _lookup :: forall eff. ChunkIndex -> ChunkMap -> Eff eff (Nullable ChunkInstance)

lookup :: forall eff. ChunkIndex -> ChunkMap -> Eff eff (Maybe ChunkInstance)
lookup index map = toMaybe <$> _lookup index map

foreign import _peekAt :: forall eff. Int -> ChunkMap -> Eff eff (Nullable ChunkInstance)

peekAt :: forall eff. Int -> ChunkMap -> Eff eff (Maybe ChunkInstance)
peekAt index map = toMaybe <$> _peekAt index map

foreign import insert :: forall eff. ChunkIndex -> ChunkInstance -> ChunkMap -> Eff eff Unit

foreign import delete :: forall eff. ChunkIndex -> ChunkMap -> Eff eff Unit

foreign import size ::  forall eff. ChunkMap -> Eff eff Int

foreign import sort :: forall eff. Int -> Int -> Int -> ChunkMap -> Eff eff Unit

foreign import slice :: forall eff. Int -> Int -> ChunkMap -> Eff eff (Array ChunkInstance)

foreign import filterNeighbors :: forall eff. Int -> Int -> Int -> Int -> ChunkMap -> Eff eff (Array ChunkInstance)

foreign import getSortedChunks :: forall eff. Int -> Int -> Int -> ChunkMap -> Eff eff (Array ChunkInstance)

foreign import toList :: forall eff. ChunkMap -> Eff eff (Array ChunkInstance)

