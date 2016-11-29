module Game.Cubbit.MeshBuilder (createTerrainGeometry, createChunkMesh, updateChunkMesh, loadDefaultChunk) where

import Control.Alternative (pure)
import Control.Bind (bind)
import Control.Monad (void)
import Control.Monad.Eff (Eff, forE)
import Control.Monad.Eff.Ref (REF, Ref, readRef)
import Data.Array (length)
import Data.Maybe (Maybe(..))
import Data.Unit (Unit, unit)
import Game.Cubbit.BlockIndex (BlockIndex, blockIndex)
import Game.Cubbit.BlockType (BlockTypes, blockTypes)
import Game.Cubbit.Chunk (Chunk(..), ChunkWithMesh, MeshLoadingState(..), VertexDataPropsData(..), disposeChunk)
import Game.Cubbit.ChunkIndex (ChunkIndex, chunkIndex, runChunkIndex)
import Game.Cubbit.Constants (chunkSize)
import Game.Cubbit.Generation (createBlockMap)
import Game.Cubbit.LocalIndex (LocalIndex)
import Game.Cubbit.MeshBuilder (createTerrainGeometry)
import Game.Cubbit.Terrain (Terrain(Terrain), globalIndexToChunkIndex, globalIndexToLocalIndex, insertChunk, lookupChunk)
import Game.Cubbit.Types (Materials, State(State))
import Graphics.Babylon (BABYLON)
import Graphics.Babylon.AbstractMesh (setMaterial, setIsPickable, setUseVertexColors, setRenderingGroupId, setReceiveShadows)
import Graphics.Babylon.Material (Material, setAlpha)
import Graphics.Babylon.Mesh (meshToAbstractMesh, createMesh)
import Graphics.Babylon.Types (Mesh, Scene)
import Graphics.Babylon.VertexData (VertexDataProps(VertexDataProps), applyToMesh, createVertexData)
import Prelude ((+), (-), (<), (=<<))

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



loadDefaultChunk :: forall eff. Ref State -> ChunkIndex -> Eff (ref :: REF, babylon :: BABYLON | eff) Boolean
loadDefaultChunk ref index = do
    State state@{ terrain: Terrain terrain } <- readRef ref
    let ci = runChunkIndex index
    boxMapMaybe <- lookupChunk index state.terrain
    case boxMapMaybe of
        Just _ -> pure false
        Nothing -> do
            blocks <- pure (createBlockMap terrain.noise index)
            insertChunk {
                x: ci.x,
                y: ci.y,
                z: ci.z,
                index,
                blocks,
                standardMaterialMesh: MeshNotLoaded,
                waterMaterialMesh: EmptyMeshLoaded
            } state.terrain
            pure true


createChunkMesh :: forall eff. Ref State -> Materials -> Scene -> ChunkIndex -> Eff (ref :: REF, babylon :: BABYLON | eff) Boolean
createChunkMesh ref materials scene index = do
    State state@{ terrain: Terrain terrain } <- readRef ref

    let i = runChunkIndex index
    forE (i.x - 1) (i.x + 2) \x -> do
        forE (i.y - 1) (i.y + 2) \y -> do
            forE (i.z - 1) (i.z + 2) \z -> void do
                loadDefaultChunk ref (chunkIndex x y z)


    boxMapMaybe <- lookupChunk index state.terrain
    blocks <- pure case boxMapMaybe of
                    Nothing -> createBlockMap terrain.noise index
                    Just m -> m.blocks

    case createTerrainGeometry (Terrain terrain) (Chunk { index, blocks }) of
        VertexDataPropsData verts@{
            standardMaterialBlocks: VertexDataProps standardMaterialBlocks,
            waterMaterialBlocks: VertexDataProps waterMaterialBlocks
        } -> do
            chunkMaybe <- lookupChunk index state.terrain
            case chunkMaybe of
                Nothing -> pure unit
                Just chunkData -> disposeChunk chunkData

            let ci = runChunkIndex index


            let gen vertices mat = if 0 < length vertices.indices
                    then do
                        mesh <- generateMesh index (VertexDataProps vertices) mat scene
                        pure (MeshLoaded mesh)
                    else do
                        pure EmptyMeshLoaded

            standardMaterialMesh <- gen standardMaterialBlocks materials.boxMat
            waterMaterialMesh <- gen waterMaterialBlocks materials.waterBoxMat



            insertChunk {
                x: ci.x,
                y: ci.y,
                z: ci.z,
                index,
                blocks,
                standardMaterialMesh,
                waterMaterialMesh
            } state.terrain

            pure (0 < (length standardMaterialBlocks.indices + length waterMaterialBlocks.indices) )

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

    let index = chunkWithMesh.index

    VertexDataPropsData verts <- pure (createTerrainGeometry state.terrain (Chunk { index, blocks: chunkWithMesh.blocks }))

    chunkDataMaybe <- lookupChunk index state.terrain
    case chunkDataMaybe of
        Nothing -> pure unit
        Just chunkData -> disposeChunk chunkData

    standardMaterialMesh <- generateMesh index verts.standardMaterialBlocks materials.boxMat scene
    waterMaterialMesh <- generateMesh index verts.waterMaterialBlocks materials.boxMat scene

    let ci = runChunkIndex index
    mesh <- pure {
        x: ci.x,
        y: ci.y,
        z: ci.z,
        index,
        blocks: chunkWithMesh.blocks,
        standardMaterialMesh: MeshLoaded standardMaterialMesh,
        waterMaterialMesh: MeshLoaded waterMaterialMesh
    }
    insertChunk mesh state.terrain

