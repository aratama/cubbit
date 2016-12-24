module Game.Cubbit.ChunkIndex (ChunkIndex, chunkIndex, runChunkIndex, chunkIndexDistance) where

import Data.Foreign.Class (class AsForeign, class IsForeign, read, write)
import Data.Generic (class Generic, gCompare, gEq)
import Data.Ord (class Ord, abs, compare, max)
import Game.Cubbit.BlockIndex (BlockIndex, blockIndex, runBlockIndex, eqBlockIndex, showBlockIndex, blockIndexDistance)
import Prelude (class Eq, class Show, show, (-), (<$>), eq, (==))

newtype ChunkIndex = ChunkIndex BlockIndex

chunkIndex :: Int -> Int -> Int -> ChunkIndex
chunkIndex x y z = ChunkIndex (blockIndex x y z)

runChunkIndex :: ChunkIndex -> { x :: Int, y :: Int, z :: Int }
runChunkIndex (ChunkIndex xyz) = runBlockIndex xyz

chunkIndexDistance :: ChunkIndex -> ChunkIndex -> Int
chunkIndexDistance (ChunkIndex a) (ChunkIndex b) = blockIndexDistance a b

instance isForeign_ChunkIndex :: IsForeign ChunkIndex where
    read value = ChunkIndex <$> read value

instance asForeign_ChunkIndex :: AsForeign ChunkIndex where
    write (ChunkIndex i) = write i

instance eqChunkIndex :: Eq ChunkIndex where
    eq (ChunkIndex a) (ChunkIndex b) = eqBlockIndex a b

instance showChunkIndex :: Show ChunkIndex where
    show (ChunkIndex a) = showBlockIndex a

instance ordChunkIndex :: Ord ChunkIndex where
    compare (ChunkIndex a) (ChunkIndex b) = compare a b
