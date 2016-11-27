module Game.Cubbit.MeshBuilder (createTerrainGeometry, createChunkMesh, updateChunkMesh) where

import Control.Alternative (pure)
import Control.Bind (bind)
import Control.Monad (void)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Ref (REF, Ref, modifyRef, readRef, writeRef)
import Data.Array (length)
import Data.Maybe (Maybe(..))
import Data.Unit (Unit, unit)
import Game.Cubbit.BlockIndex (BlockIndex, blockIndex)
import Game.Cubbit.BlockType (BlockTypes, blockTypes)
import Game.Cubbit.Chunk (Chunk(..), ChunkWithMesh, MeshLoadingState(..))
import Game.Cubbit.ChunkIndex (ChunkIndex, runChunkIndex)
import Game.Cubbit.Constants (chunkSize)
import Game.Cubbit.Generation (createBlockMap)
import Game.Cubbit.LocalIndex (LocalIndex)
import Game.Cubbit.MeshBuilder (createTerrainGeometry)
import Game.Cubbit.Terrain (Terrain(Terrain), disposeChunk, globalIndexToChunkIndex, globalIndexToLocalIndex, insertChunk, lookupChunk)
import Game.Cubbit.Types (Materials, State(State))
import Game.Cubbit.VertexDataPropsData (VertexDataPropsData(..))
import Graphics.Babylon (BABYLON)
import Graphics.Babylon.AbstractMesh (setMaterial, setIsPickable, setUseVertexColors, setRenderingGroupId, setReceiveShadows)
import Graphics.Babylon.Material (Material)
import Graphics.Babylon.Mesh (meshToAbstractMesh, createMesh)
import Graphics.Babylon.Types (Mesh, Scene)
import Graphics.Babylon.VertexData (VertexDataProps(VertexDataProps), applyToMesh, createVertexData)
import Prelude (($), (=<<), (<))

type CreateTerrainGeometryReferences = {
    chunkSize :: Int,
    blockTypes :: BlockTypes,
    runChunkIndex :: ChunkIndex -> { x :: Int, y :: Int, z :: Int },
    blockIndex :: Int -> Int -> Int -> BlockIndex,
    globalIndexToChunkIndex :: BlockIndex -> ChunkIndex,
    globalIndexToLocalIndex :: BlockIndex -> LocalIndex
}

createTerrainGeometryReferences :: CreateTerrainGeometryReferences
createTerrainGeometryReferences = {
    chunkSize: chunkSize,
    blockTypes: blockTypes,
    runChunkIndex: runChunkIndex,
    blockIndex: blockIndex,
    globalIndexToChunkIndex: globalIndexToChunkIndex,
    globalIndexToLocalIndex: globalIndexToLocalIndex
}

foreign import createTerrainGeometryJS :: CreateTerrainGeometryReferences -> Terrain -> Chunk -> VertexDataPropsData

createTerrainGeometry :: Terrain -> Chunk -> VertexDataPropsData
createTerrainGeometry = createTerrainGeometryJS createTerrainGeometryReferences

createChunkMesh :: forall eff. Ref State -> Materials -> Scene -> ChunkIndex -> Eff (ref :: REF, babylon :: BABYLON | eff) Boolean
createChunkMesh ref materials scene index = do
    State state@{ terrain: Terrain terrain } <- readRef ref

    boxMapMaybe <- lookupChunk index state.terrain
    let boxMap = case boxMapMaybe of
                    Nothing -> createBlockMap terrain.noise index
                    Just m -> m.blocks

    case createTerrainGeometry (Terrain terrain) boxMap of
        VertexDataPropsData verts@{ standardMaterialBlocks: VertexDataProps vertices } -> do
            chunkMaybe <- lookupChunk index state.terrain
            case chunkMaybe of
                Nothing -> pure unit
                Just chunkData -> disposeChunk chunkData

            let ci = runChunkIndex index
            result <- if 0 < length vertices.indices
                then do
                    standardMaterialMesh <- generateMesh index verts.standardMaterialBlocks materials.boxMat scene
                    pure (MeshLoaded standardMaterialMesh)
                else do
                    pure EmptyMeshLoaded
            insertChunk {
                x: ci.x,
                y: ci.y,
                z: ci.z,
                blocks: verts.terrain,
                standardMaterialMesh: result
            } state.terrain

            pure (0 < length vertices.indices)

generateMesh :: forall eff. ChunkIndex -> VertexDataProps -> Material -> Scene -> Eff (babylon :: BABYLON | eff) Mesh
generateMesh index verts mat scene = do
    let rci = runChunkIndex index
    let cx = rci.x
    let cy = rci.y
    let cz = rci.z
    terrainMesh <- createMesh "terrain" scene
    applyToMesh terrainMesh false =<< createVertexData (verts)
    setRenderingGroupId 1 (meshToAbstractMesh terrainMesh)
    setReceiveShadows true (meshToAbstractMesh terrainMesh)
    setUseVertexColors true (meshToAbstractMesh terrainMesh)
    setMaterial mat (meshToAbstractMesh terrainMesh)
    setIsPickable false (meshToAbstractMesh terrainMesh)
    pure terrainMesh


updateChunkMesh :: forall eff. Ref State -> Materials -> Scene -> ChunkWithMesh -> Eff (ref :: REF, babylon :: BABYLON | eff) Unit
updateChunkMesh ref materials scene chunkWithMesh = void do

    State state <- readRef ref

    VertexDataPropsData verts@{ terrain: Chunk chunk } <- pure (createTerrainGeometry state.terrain chunkWithMesh.blocks)

    let index = chunk.index

    chunkDataMaybe <- lookupChunk chunk.index state.terrain
    case chunkDataMaybe of
        Nothing -> pure unit
        Just chunkData -> disposeChunk chunkData

    standardMaterialMesh <- generateMesh index verts.standardMaterialBlocks materials.boxMat scene
    let ci = runChunkIndex index
    mesh <- pure {
        x: ci.x,
        y: ci.y,
        z: ci.z,
        blocks: verts.terrain,
        standardMaterialMesh: MeshLoaded standardMaterialMesh
    }
    insertChunk mesh state.terrain

