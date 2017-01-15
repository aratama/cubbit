module Game.Cubbit.Chunk (Chunk(..)) where

import Game.Cubbit.BoxelMap (BoxelMap)
import Game.Cubbit.ChunkIndex (ChunkIndex)

newtype Chunk = Chunk {
    index :: ChunkIndex,
    blocks :: BoxelMap
}

