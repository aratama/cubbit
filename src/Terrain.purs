module Graphics.Babylon.Example.Sandbox.Terrain (
 ChunkWithMesh(..), Terrain, emptyTerrain,
 globalPositionToChunkIndex, globalPositionToLocalIndex, globalPositionToGlobalIndex, globalIndexToChunkIndex, globalIndexToLocalIndex,
 lookupBlock, insertChunk, lookupChunk, disposeChunk, chunkCount, getChunkMap
) where

import Control.Bind (bind)
import Control.Monad.Eff (Eff)
import Data.EuclideanRing (mod)
import Data.Int (floor, toNumber)
import Data.Maybe (Maybe(..))
import Data.ShowMap (ShowMap, empty, insert, lookup, size)
import Data.Unit (Unit)
import Graphics.Babylon (BABYLON)
import Graphics.Babylon.AbstractMesh (dispose)
import Graphics.Babylon.Example.Sandbox.BlockIndex (BlockIndex, blockIndex, runBlockIndex)
import Graphics.Babylon.Example.Sandbox.BlockType (BlockType, airBlock)
import Graphics.Babylon.Example.Sandbox.BoxelMap (lookup) as Boxel
import Graphics.Babylon.Example.Sandbox.Chunk (Chunk(..))
import Graphics.Babylon.Example.Sandbox.ChunkIndex (ChunkIndex, chunkIndex, runChunkIndex)
import Graphics.Babylon.Example.Sandbox.Constants (chunkSize)
import Graphics.Babylon.Example.Sandbox.LocalIndex (LocalIndex, localIndex)
import Graphics.Babylon.Example.Sandbox.Vec (Vec)
import Graphics.Babylon.Mesh (meshToAbstractMesh)
import Graphics.Babylon.Types (Mesh)
import Prelude ((*), (/), (+), (-), ($), (==))


type ChunkWithMesh = {
    blocks :: Chunk,
    standardMaterialMesh :: Mesh
}

newtype Terrain = Terrain { map :: ShowMap ChunkIndex ChunkWithMesh }

getChunkMap :: Terrain -> ShowMap ChunkIndex ChunkWithMesh
getChunkMap (Terrain t) = t.map

emptyTerrain :: Terrain
emptyTerrain = Terrain { map: empty }

chunkCount :: Terrain -> Int
chunkCount (Terrain t) = size t.map

globalPositionToChunkIndex :: Number -> Number -> Number -> ChunkIndex
globalPositionToChunkIndex x y z = chunkIndex (f x) (f y) (f z)
  where
    f v = floor (v + 1000000.0 * toNumber chunkSize) / chunkSize - 1000000

globalPositionToLocalIndex :: Number -> Number -> Number -> BlockIndex
globalPositionToLocalIndex x y z = blockIndex (f x) (f y) (f z)
  where
    delta = toNumber chunkSize * 1000000.0
    f v = mod (floor (v + delta)) chunkSize

globalPositionToGlobalIndex :: Number -> Number -> Number -> BlockIndex
globalPositionToGlobalIndex x y z = blockIndex (f x) (f y) (f z)
  where
    f v = floor (v + 1000000.0) - 1000000

globalIndexToChunkIndex :: BlockIndex -> ChunkIndex
globalIndexToChunkIndex b = chunkIndex (f bi.x) (f bi.y) (f bi.z)
  where
    bi = runBlockIndex b
    f v = (v + 1000000 * chunkSize) / chunkSize - 1000000



lookupChunk :: ChunkIndex -> Terrain -> Maybe ChunkWithMesh
lookupChunk index (Terrain terrain) = lookup index terrain.map

lookupBlock :: Vec -> Terrain -> Maybe BlockType
lookupBlock p (Terrain terrain) = do
        let chunkIndex = globalPositionToChunkIndex p.x p.y p.z
        let globalIndex = globalPositionToGlobalIndex p.x p.y p.z
        let localIndex = globalIndexToLocalIndex globalIndex
        { blocks: Chunk chunk@{ blocks } } <- lookup chunkIndex terrain.map
        blockType <- Boxel.lookup localIndex blocks
        if blockType == airBlock then Nothing else Just blockType

globalIndexToLocalIndex :: BlockIndex -> LocalIndex
globalIndexToLocalIndex index = localIndex cx cy cz
  where
    chunkIndex = runChunkIndex (globalIndexToChunkIndex index)
    globalIndex = runBlockIndex index
    cx = globalIndex.x - chunkSize * chunkIndex.x
    cy = globalIndex.y - chunkSize * chunkIndex.y
    cz = globalIndex.z - chunkSize * chunkIndex.z

insertChunk :: ChunkWithMesh -> Terrain -> Terrain
insertChunk cmesh@{ blocks: Chunk chunk@{ index } } (Terrain chunks) = Terrain chunks {
    map = insert index cmesh chunks.map
}

disposeChunk :: forall eff. ChunkWithMesh -> Eff (babylon :: BABYLON | eff) Unit
disposeChunk chunk = do
    dispose true $ meshToAbstractMesh chunk.standardMaterialMesh
