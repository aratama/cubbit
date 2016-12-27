module Game.Cubbit.ChunkInstance (MeshLoadingState(..), ChunkInstance, createChunkWithMesh, VertexDataPropsData(..), disposeLoadedMesh, disposeChunk) where

import Control.Monad.Eff (Eff)
import Data.Unit (Unit, unit)
import Game.Cubbit.BoxelMap (BoxelMap) as Boxel
import Game.Cubbit.ChunkIndex (ChunkIndex, runChunkIndex)
import Game.Cubbit.Chunk (Chunk(..))
import Graphics.Babylon.AbstractMesh (dispose)
import Graphics.Babylon.Mesh (meshToAbstractMesh)
import Graphics.Babylon.Types (BABYLON, Mesh, VertexDataProps)
import Prelude (pure, bind)

data MeshLoadingState = MeshNotLoaded | MeshLoaded Mesh | EmptyMeshLoaded

type ChunkInstance = {
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

createChunkWithMesh :: Chunk -> Boolean -> ChunkInstance
createChunkWithMesh (Chunk chunk) edited = {
    x: i.x,
    y: i.y,
    z: i.z,
    index: chunk.index,
    blocks: chunk.blocks,
    edited,
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

disposeChunk :: forall eff. ChunkInstance -> Eff (babylon :: BABYLON | eff) Unit
disposeChunk chunk = do
    disposeLoadedMesh chunk.standardMaterialMesh
    disposeLoadedMesh chunk.waterMaterialMesh
    disposeLoadedMesh chunk.transparentMaterialMesh
