module Game.Cubbit.MeshBuilder (generateChunk, editBlock, putBlocks) where

import Control.Alternative (pure)
import Control.Bind (bind)
import Control.Monad (void, when)
import Control.Monad.Eff (Eff, forE)
import DOM (DOM)
import Data.Array (length)
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (for_)
import Data.Unit (Unit, unit)
import Game.Cubbit.BlockIndex (BlockIndex, blockIndex)
import Game.Cubbit.BlockType (BlockType, BlockTypes, blockTypes)
import Game.Cubbit.BoxelMap (insert)
import Game.Cubbit.Chunk (Chunk(..))
import Game.Cubbit.ChunkIndex (ChunkIndex, chunkIndex, runChunkIndex)
import Game.Cubbit.ChunkInstance (ChunkInstance, MeshLoadingState(..), VertexDataPropsData(..), disposeChunk)
import Game.Cubbit.Collesion (disposeCollesion)
import Game.Cubbit.Config (Config(..))
import Game.Cubbit.Constants (chunkSize, terrainRenderingGroup)
import Game.Cubbit.Firebase (saveChunkToFirebase)
import Game.Cubbit.Generation (createBlockMap)
import Game.Cubbit.LocalIndex (LocalIndex, runLocalIndex)
import Game.Cubbit.Materials (Materials)
import Game.Cubbit.Option (Options(Options))
import Game.Cubbit.Resources (Resources)
import Game.Cubbit.Storage (saveChunk)
import Game.Cubbit.Terrain (Terrain(Terrain), globalIndexToChunkIndex, globalIndexToLocalIndex, insertChunk, lookupChunk)
import Game.Cubbit.Types (State(State), SceneState(..), GameMode(..))
import Graphics.Babylon.AbstractMesh (setMaterial, setIsPickable, setUseVertexColors, setRenderingGroupId, setReceiveShadows)
import Graphics.Babylon.Mesh (meshToAbstractMesh, createMesh)
import Graphics.Babylon.Types (VertexDataProps(VertexDataProps), Material, BABYLON, Mesh, Scene)
import Graphics.Babylon.VertexData (applyToMesh, createVertexData)
import Graphics.Cannon (CANNON)
import PerlinNoise (Noise, simplex2)
import Prelude (negate, (+), (-), (<), (=<<), (==), (>>=))
import Web.Firebase (FIREBASE)


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

generateNeighborChunks :: forall eff. Terrain -> ChunkIndex -> Eff (babylon :: BABYLON | eff) Unit
generateNeighborChunks terrain@(Terrain t) index = do
    let i = runChunkIndex index
    forE (i.x - 1) (i.x + 2) \x -> do
        forE (i.y - 1) (i.y + 2) \y -> do
            forE (i.z - 1) (i.z + 2) \z -> void do
                let loadingChunkIndex = chunkIndex x y z
                boxMapMaybe <- lookupChunk loadingChunkIndex terrain
                case boxMapMaybe of
                    Just _ -> pure unit
                    Nothing -> insertChunk {
                        x: x,
                        y: y,
                        z: z,
                        index: loadingChunkIndex,
                        blocks: createBlockMap t.noise loadingChunkIndex,
                        edited: false,

                        standardMaterialMesh: MeshNotLoaded,
                        waterMaterialMesh: MeshNotLoaded,
                        transparentMaterialMesh: MeshNotLoaded
                    } terrain

generateChunk :: forall eff. State -> Materials -> Scene -> ChunkIndex -> Options -> Config -> Resources -> Eff (babylon :: BABYLON | eff) Boolean
generateChunk (State state@{ terrain: Terrain terrain }) materials scene index (Options options) (Config config) res = do


    -- generate terrain -------------------------------
    generateNeighborChunks state.terrain index

    -- generate mesh ------------------------
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
                        mesh <- generateMesh index (VertexDataProps vertices) mat scene (Config config)
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
                edited: false,

                standardMaterialMesh,
                waterMaterialMesh,
                transparentMaterialMesh
            } state.terrain

            pure (0 < (length standardMaterialBlocks.indices + length waterMaterialBlocks.indices) )

generateMesh :: forall eff. ChunkIndex -> VertexDataProps -> Material -> Scene -> Config -> Eff (babylon :: BABYLON | eff) Mesh
generateMesh index verts mat scene (Config config) = do
    let rci = runChunkIndex index
    let cx = rci.x
    let cy = rci.y
    let cz = rci.z
    terrainMesh <- createMesh "terrain" scene
    applyToMesh terrainMesh false =<< createVertexData verts
    setRenderingGroupId terrainRenderingGroup (meshToAbstractMesh terrainMesh)
    setReceiveShadows config.shadow (meshToAbstractMesh terrainMesh)
    setUseVertexColors config.vertexColor (meshToAbstractMesh terrainMesh)
    setMaterial mat (meshToAbstractMesh terrainMesh)
    setIsPickable false (meshToAbstractMesh terrainMesh)
    pure terrainMesh




editBlock :: forall eff. State -> BlockIndex -> BlockType -> Resources -> Eff (dom :: DOM, babylon :: BABYLON, firebase :: FIREBASE | eff) Unit
editBlock (State state) globalBlockIndex block res = do
    let editChunkIndex = globalIndexToChunkIndex globalBlockIndex
    chunkMaybe <- lookupChunk editChunkIndex state.terrain
    case chunkMaybe of

        Just chunkData -> void do

            -- edit the chunk --
            let localIndex = globalIndexToLocalIndex globalBlockIndex
            let li = runLocalIndex localIndex
            let blocks = insert localIndex block chunkData.blocks
            let chunkData' = chunkData {
                        blocks = blocks,
                        edited = true
                    }
            updateChunkMesh (State state) res.materials res.scene chunkData' res.options state.config

            -- update neighbors --
            let eci = runChunkIndex editChunkIndex
            let refreash dx dy dz = do
                    let refreashIndex = chunkIndex (eci.x + dx) (eci.y + dy) (eci.z + dz)
                    if refreashIndex == editChunkIndex
                        then pure unit
                        else do
                            targetChunkMaybe <- lookupChunk refreashIndex state.terrain
                            for_ targetChunkMaybe \targetChunkData -> updateChunkMesh (State state) res.materials res.scene targetChunkData res.options state.config

            when (li.x == 0) (refreash (-1) 0 0)
            when (li.x == chunkSize - 1) (refreash 1 0 0)
            when (li.y == 0) (refreash 0 (-1) 0)
            when (li.y == chunkSize - 1) (refreash 0 1 0)
            when (li.z == 0) (refreash 0 0 (-1))
            when (li.z == chunkSize - 1) (refreash 0 0 1)

            -- save chunk data to storage --
            let serverChunkData = Chunk { index: chunkData.index, blocks: blocks }
            case state.sceneState of
                PlayingSceneState p -> do
                    case p.gameMode of
                        SinglePlayerMode -> saveChunk serverChunkData
                        MultiplayerMode -> saveChunkToFirebase serverChunkData res.firebase
                _ -> pure unit -- never come here


        _ -> pure unit





putBlocks :: forall eff. State -> Resources -> Chunk -> Eff (dom :: DOM, babylon :: BABYLON, cannon :: CANNON | eff) Terrain
putBlocks (State state) res (Chunk chunk) = do
    lookupChunk chunk.index state.terrain >>= maybe (pure state.terrain) \chunkData -> do
        -- generate neighbor terrain -------------------------------
        generateNeighborChunks state.terrain chunk.index

        -- overwrite the chunk
        updateChunkMesh (State state) res.materials res.scene chunkData {
            blocks = chunk.blocks,
            edited = true
        } res.options state.config

        -- update collesion
        disposeCollesion state.terrain res.world chunk.index






updateChunkMesh :: forall eff. State -> Materials -> Scene -> ChunkInstance -> Options -> Config -> Eff (babylon :: BABYLON | eff) Unit
updateChunkMesh (State state) materials scene chunkWithMesh (Options options) (Config config) = void do

    let index = chunkWithMesh.index

    VertexDataPropsData verts <- pure (createTerrainGeometry state.terrain (Chunk { index, blocks: chunkWithMesh.blocks }))

    chunkDataMaybe <- lookupChunk index state.terrain
    case chunkDataMaybe of
        Nothing -> pure unit
        Just chunkData -> disposeChunk chunkData

    standardMaterialMesh <- generateMesh index verts.standardMaterialBlocks materials.blockMaterial scene (Config config)
    waterMaterialMesh <- generateMesh index verts.waterMaterialBlocks materials.waterMaterial scene (Config config)
    transparentMaterialMesh <- generateMesh index verts.transparentMaterialVertexData materials.bushMaterial scene (Config config)

    let ci = runChunkIndex index
    mesh <- pure {
        x: ci.x,
        y: ci.y,
        z: ci.z,
        index,
        blocks: chunkWithMesh.blocks,
        edited: chunkWithMesh.edited,

        standardMaterialMesh: MeshLoaded standardMaterialMesh,
        waterMaterialMesh: MeshLoaded waterMaterialMesh,
        transparentMaterialMesh: MeshLoaded transparentMaterialMesh
    }
    insertChunk mesh state.terrain


