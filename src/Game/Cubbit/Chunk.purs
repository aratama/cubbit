module Game.Cubbit.Chunk where

import Control.Monad.Eff (Eff)
import Data.Foreign (toForeign, unsafeFromForeign)
import Data.Foreign.Class (class AsForeign, class IsForeign)
import Data.Unit (Unit, unit)
import Game.Cubbit.BoxelMap (BoxelMap) as Boxel
import Game.Cubbit.ChunkIndex (ChunkIndex, runChunkIndex)
import Graphics.Babylon.AbstractMesh (dispose)
import Graphics.Babylon.Mesh (meshToAbstractMesh)
import Graphics.Babylon.Types (BABYLON, Mesh, VertexDataProps)
import Prelude (pure, bind)

newtype Chunk = Chunk {
    index :: ChunkIndex,
    blocks :: Boxel.BoxelMap
}

instance isForeign_TerrainMap :: IsForeign Chunk where
    read value = pure (unsafeFromForeign value)

instance asForeign_TerrainMap :: AsForeign Chunk where
    write = toForeign


data MeshLoadingState = MeshNotLoaded | MeshLoaded Mesh | EmptyMeshLoaded

type ChunkWithMesh = {
    x :: Int,
    y :: Int,
    z :: Int,
    index :: ChunkIndex,
    blocks :: Boxel.BoxelMap,
    edited :: Boolean,

    standardMaterialMesh :: MeshLoadingState,
    waterMaterialMesh :: MeshLoadingState,
    transparentMaterialMesh :: MeshLoadingState
}

createChunkWithMesh :: Chunk -> ChunkWithMesh
createChunkWithMesh (Chunk chunk) = {
    x: i.x,
    y: i.y,
    z: i.z,
    index: chunk.index,
    blocks: chunk.blocks,
    edited: false,
    standardMaterialMesh: MeshNotLoaded,
    waterMaterialMesh: MeshNotLoaded,
    transparentMaterialMesh: MeshNotLoaded
}
  where
    i = runChunkIndex chunk.index

newtype VertexDataPropsData = VertexDataPropsData {
    standardMaterialBlocks :: VertexDataProps,
    waterMaterialBlocks :: VertexDataProps,
    transparentMaterialVertexData :: VertexDataProps
}

disposeLoadedMesh :: forall eff. MeshLoadingState -> Eff (babylon :: BABYLON | eff) Unit
disposeLoadedMesh = case _ of
    MeshNotLoaded -> pure unit
    MeshLoaded mesh -> dispose true (meshToAbstractMesh mesh)
    EmptyMeshLoaded -> pure unit

disposeChunk :: forall eff. ChunkWithMesh -> Eff (babylon :: BABYLON | eff) Unit
disposeChunk chunk = do
    disposeLoadedMesh chunk.standardMaterialMesh
    disposeLoadedMesh chunk.waterMaterialMesh
    disposeLoadedMesh chunk.transparentMaterialMesh
