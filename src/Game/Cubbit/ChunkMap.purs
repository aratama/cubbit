module Game.Cubbit.ChunkMap where

import Control.Monad.Eff (Eff)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Data.Unit (Unit)
import Game.Cubbit.Chunk (ChunkWithMesh)
import Game.Cubbit.ChunkIndex (ChunkIndex)
import Prelude ((<$>))

foreign import data ChunkMap :: *

foreign import createChunkMap :: forall eff. Eff eff ChunkMap

foreign import _lookup :: forall eff. ChunkIndex -> ChunkMap -> Eff eff (Nullable ChunkWithMesh)

lookup :: forall eff. ChunkIndex -> ChunkMap -> Eff eff (Maybe ChunkWithMesh)
lookup index map = toMaybe <$> _lookup index map

foreign import insert :: forall eff. ChunkIndex -> ChunkWithMesh -> ChunkMap -> Eff eff Unit

foreign import delete :: forall eff. ChunkIndex -> ChunkMap -> Eff eff Unit

foreign import size ::  forall eff. ChunkMap -> Eff eff Int
