module Game.Cubbit.Chunk where

import Data.Foreign (toForeign, unsafeFromForeign)
import Data.Foreign.Class (class AsForeign, class IsForeign)
import Game.Cubbit.BlockType (BlockType)
import Game.Cubbit.BoxelMap (BoxelMap) as Boxel
import Game.Cubbit.ChunkIndex (ChunkIndex)
import Graphics.Babylon.Types (Mesh)
import Prelude (pure)

newtype Chunk = Chunk {
    index :: ChunkIndex,
    blocks :: Boxel.BoxelMap BlockType
}

instance isForeign_TerrainMap :: IsForeign Chunk where
    read value = pure (unsafeFromForeign value)

instance asForeign_TerrainMap :: AsForeign Chunk where
    write = toForeign


data MeshLoadingState = MeshNotLoaded | MeshLoaded Mesh | EmptyMeshLoaded

type ChunkWithMesh = {
    blocks :: Chunk,
    standardMaterialMesh :: MeshLoadingState
}
