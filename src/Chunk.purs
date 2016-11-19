module Graphics.Babylon.Example.Sandbox.Chunk where

import Data.Foreign (toForeign, unsafeFromForeign)
import Data.Foreign.Class (class AsForeign, class IsForeign)
import Graphics.Babylon.Example.Sandbox.BlockType (BlockType)
import Graphics.Babylon.Example.Sandbox.BoxelMap (BoxelMap) as Boxel
import Graphics.Babylon.Example.Sandbox.ChunkIndex (ChunkIndex)
import Prelude (pure)

newtype Chunk = Chunk {
    index :: ChunkIndex,
    blocks :: Boxel.BoxelMap BlockType
}

instance isForeign_TerrainMap :: IsForeign Chunk where
    read value = pure (unsafeFromForeign value)

instance asForeign_TerrainMap :: AsForeign Chunk where
    write = toForeign
