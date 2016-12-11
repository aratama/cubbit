module Game.Cubbit.MeshBuilder (createTerrainGeometry, createChunkMesh, loadDefaultChunk, editBlock) where

import Control.Alternative (pure)
import Control.Bind (bind)
import Control.Monad (void, when)
import Control.Monad.Eff (Eff, forE)
import Control.Monad.Eff.Ref (REF, Ref, readRef)
import Data.Array (length)
import Data.Maybe (Maybe(..))
import Data.Unit (Unit, unit)
import Game.Cubbit.BlockIndex (BlockIndex, blockIndex)
import Game.Cubbit.BlockType (BlockType, BlockTypes, blockTypes)
import Game.Cubbit.BoxelMap (insert)
import Game.Cubbit.Chunk (Chunk(..), ChunkWithMesh, MeshLoadingState(..), VertexDataPropsData(..), disposeChunk)
import Game.Cubbit.ChunkIndex (ChunkIndex, chunkIndex, runChunkIndex)
import Game.Cubbit.Constants (chunkSize, terrainRenderingGroup)
import Game.Cubbit.Generation (createBlockMap)
import Game.Cubbit.LocalIndex (LocalIndex, runLocalIndex)
import Game.Cubbit.MeshBuilder (createTerrainGeometry)
import Game.Cubbit.Materials (Materials)
import Game.Cubbit.Terrain (Terrain(Terrain), globalIndexToChunkIndex, globalIndexToLocalIndex, insertChunk, lookupChunk)
import Game.Cubbit.Types (State(State))
import Game.Cubbit.Option (Options(Options))
import Graphics.Babylon.AbstractMesh (setMaterial, setIsPickable, setUseVertexColors, setRenderingGroupId, setReceiveShadows)
import Graphics.Babylon.Mesh (meshToAbstractMesh, createMesh)
import Graphics.Babylon.Types (VertexDataProps(VertexDataProps), Material, BABYLON, Mesh, Scene)
import Graphics.Babylon.VertexData (applyToMesh, createVertexData)
import PerlinNoise (Noise, simplex2)
import Prelude ((+), (-), (<), (=<<), (==), negate)

type CreateTerrainGeometryReferences = {
    chunkSize :: Int,
    blockTypes :: BlockTypes,
    runChunkIndex :: ChunkIndex -> { x :: Int, y :: Int, z :: Int },
    blockIndex :: Int -> Int -> Int -> BlockIndex,
    globalIndexToChunkIndex :: BlockIndex -> ChunkIndex,
    globalIndexToLocalIndex :: BlockIndex -> LocalIndex,
    simplex2 :: Number -> Number -> Noise -> Number
}

createTerrainGeometryReferences :: CreateTerrainGeometryReferences
createTerrainGeometryReferences = {
    chunkSize: chunkSize,
    blockTypes: blockTypes,
    runChunkIndex: runChunkIndex,
    blockIndex: blockIndex,
    globalIndexToChunkIndex: globalIndexToChunkIndex,
    globalIndexToLocalIndex: globalIndexToLocalIndex,
    simplex2: simplex2
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
                waterMaterialMesh: MeshNotLoaded,
                transparentMaterialMesh: MeshNotLoaded
            } state.terrain
            pure true


createChunkMesh :: forall eff. Ref State -> Materials -> Scene -> ChunkIndex -> Options -> Eff (ref :: REF, babylon :: BABYLON | eff) Boolean
createChunkMesh ref materials scene index (Options options) = do
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
            waterMaterialBlocks: VertexDataProps waterMaterialBlocks,
            transparentMaterialVertexData: VertexDataProps transparentMaterialVertexData
        } -> do
            chunkMaybe <- lookupChunk index state.terrain
            case chunkMaybe of
                Nothing -> pure unit
                Just chunkData -> disposeChunk chunkData

            let ci = runChunkIndex index


            let gen vertices mat gruop = if 0 < length vertices.indices
                    then do
                        mesh <- generateMesh index (VertexDataProps vertices) mat scene (Options options)
                        setRenderingGroupId gruop (meshToAbstractMesh mesh)
                        pure (MeshLoaded mesh)
                    else do
                        pure EmptyMeshLoaded

            standardMaterialMesh <- gen standardMaterialBlocks materials.blockMaterial terrainRenderingGroup
            waterMaterialMesh <- gen waterMaterialBlocks materials.waterMaterial terrainRenderingGroup
            transparentMaterialMesh <- gen transparentMaterialVertexData materials.bushMaterial terrainRenderingGroup

            insertChunk {
                x: ci.x,
                y: ci.y,
                z: ci.z,
                index,
                blocks,
                standardMaterialMesh,
                waterMaterialMesh,
                transparentMaterialMesh
            } state.terrain

            pure (0 < (length standardMaterialBlocks.indices + length waterMaterialBlocks.indices) )

generateMesh :: forall eff. ChunkIndex -> VertexDataProps -> Material -> Scene -> Options -> Eff (babylon :: BABYLON | eff) Mesh
generateMesh index verts mat scene (Options options) = do
    let rci = runChunkIndex index
    let cx = rci.x
    let cy = rci.y
    let cz = rci.z
    terrainMesh <- createMesh "terrain" scene
    applyToMesh terrainMesh false =<< createVertexData (verts)
    setRenderingGroupId terrainRenderingGroup (meshToAbstractMesh terrainMesh)
    setReceiveShadows options.shadowEnabled (meshToAbstractMesh terrainMesh)
    setUseVertexColors options.vertexColorEnabled (meshToAbstractMesh terrainMesh)
    setMaterial mat (meshToAbstractMesh terrainMesh)
    setIsPickable false (meshToAbstractMesh terrainMesh)
    pure terrainMesh




editBlock :: forall eff. Ref State -> Materials -> Scene -> BlockIndex -> BlockType -> Options -> Eff (ref :: REF, babylon :: BABYLON | eff) Unit
editBlock ref materials scene globalBlockIndex block (Options options) = do
    State state <- readRef ref
    let editChunkIndex = globalIndexToChunkIndex globalBlockIndex
    chunkMaybe <- lookupChunk editChunkIndex state.terrain
    case chunkMaybe of
        Nothing -> pure unit
        Just chunkData -> void do
            let localIndex = globalIndexToLocalIndex globalBlockIndex
            let li = runLocalIndex localIndex
            updateChunkMesh ref materials scene chunkData {
                blocks = insert localIndex block chunkData.blocks
            } (Options options)

            let eci = runChunkIndex editChunkIndex

            let refreash dx dy dz = do
                    chunkMaybe <- lookupChunk (chunkIndex (eci.x + dx) (eci.y + dy) (eci.z + dz)) state.terrain
                    case chunkMaybe of
                        Nothing -> pure unit
                        Just chunkData -> updateChunkMesh ref materials scene chunkData (Options options)

            when (li.x == 0) (refreash (-1) 0 0)
            when (li.x == chunkSize - 1) (refreash 1 0 0)
            when (li.y == 0) (refreash 0 (-1) 0)
            when (li.y == chunkSize - 1) (refreash 0 1 0)
            when (li.z == 0) (refreash 0 0 (-1))
            when (li.z == chunkSize - 1) (refreash 0 0 1)

updateChunkMesh :: forall eff. Ref State -> Materials -> Scene -> ChunkWithMesh -> Options -> Eff (ref :: REF, babylon :: BABYLON | eff) Unit
updateChunkMesh ref materials scene chunkWithMesh (Options options) = void do

    State state <- readRef ref

    let index = chunkWithMesh.index

    VertexDataPropsData verts <- pure (createTerrainGeometry state.terrain (Chunk { index, blocks: chunkWithMesh.blocks }))

    chunkDataMaybe <- lookupChunk index state.terrain
    case chunkDataMaybe of
        Nothing -> pure unit
        Just chunkData -> disposeChunk chunkData

    standardMaterialMesh <- generateMesh index verts.standardMaterialBlocks materials.blockMaterial scene (Options options)
    waterMaterialMesh <- generateMesh index verts.waterMaterialBlocks materials.waterMaterial scene (Options options)
    transparentMaterialMesh <- generateMesh index verts.transparentMaterialVertexData materials.bushMaterial scene (Options options)

    let ci = runChunkIndex index
    mesh <- pure {
        x: ci.x,
        y: ci.y,
        z: ci.z,
        index,
        blocks: chunkWithMesh.blocks,
        standardMaterialMesh: MeshLoaded standardMaterialMesh,
        waterMaterialMesh: MeshLoaded waterMaterialMesh,
        transparentMaterialMesh: MeshLoaded transparentMaterialMesh
    }
    insertChunk mesh state.terrain


