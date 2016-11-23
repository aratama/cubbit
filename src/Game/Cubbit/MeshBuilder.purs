module Game.Cubbit.MeshBuilder (createTerrainGeometry, updateChunkMesh, createChunkMesh) where

import Control.Alternative (pure)
import Control.Bind (bind)
import Control.Monad (void)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Ref (REF, Ref, modifyRef, readRef, writeRef)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Unit (Unit, unit)
import Game.Cubbit.BlockType (BlockTypes, blockTypes)
import Game.Cubbit.Chunk (Chunk(..))
import Game.Cubbit.ChunkIndex (ChunkIndex, runChunkIndex)
import Game.Cubbit.Constants (chunkSize)
import Game.Cubbit.Generation (createBlockMap)
import Game.Cubbit.MeshBuilder (createTerrainGeometry)
import Game.Cubbit.Terrain (ChunkWithMesh, Terrain(Terrain), disposeChunk, insertChunk, lookupChunk)
import Game.Cubbit.Types (Materials, State(State))
import Game.Cubbit.VertexDataPropsData (VertexDataPropsData(..))
import Graphics.Babylon (BABYLON)
import Graphics.Babylon.AbstractMesh (setUseVertexColors, setRenderingGroupId, setReceiveShadows)
import Graphics.Babylon.Material (Material)
import Graphics.Babylon.Mesh (meshToAbstractMesh, createMesh, setMaterial)
import Graphics.Babylon.Types (Mesh, Scene)
import Graphics.Babylon.VertexData (VertexDataProps, applyToMesh, createVertexData)
import Prelude (($), (=<<))

type CreateTerrainGeometryReferences = {
    chunkSize :: Int,
    blockTypes :: BlockTypes,
    runChunkIndex :: ChunkIndex -> { x :: Int, y :: Int, z :: Int }
}

createTerrainGeometryReferences :: CreateTerrainGeometryReferences
createTerrainGeometryReferences = {
    chunkSize: chunkSize,
    blockTypes: blockTypes,
    runChunkIndex: runChunkIndex
}

foreign import createTerrainGeometryJS :: CreateTerrainGeometryReferences -> Chunk -> VertexDataPropsData

createTerrainGeometry :: Chunk -> VertexDataPropsData
createTerrainGeometry = createTerrainGeometryJS createTerrainGeometryReferences

createChunkMesh :: forall eff. Ref State -> Materials -> Scene -> ChunkIndex -> Eff (ref :: REF, babylon :: BABYLON | eff) Unit
createChunkMesh ref materials scene index = do
    State state@{ terrain: Terrain terrain } <- readRef ref
    let boxMap = createBlockMap terrain.noise index
    case createTerrainGeometry boxMap of
        VertexDataPropsData verts -> void do
            case lookupChunk index state.terrain of
                Nothing -> pure unit
                Just chunkData -> disposeChunk chunkData
            standardMaterialMesh <- generateMesh index verts.standardMaterialBlocks materials.boxMat scene
            let result = { blocks: verts.terrain, standardMaterialMesh }
            modifyRef ref \(State state) -> State state {
                terrain = insertChunk result state.terrain
            }

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
    setMaterial mat terrainMesh
    pure terrainMesh



updateChunkMesh :: forall eff. Ref State -> Materials -> Scene -> ChunkWithMesh -> Eff (ref :: REF, babylon :: BABYLON | eff) Unit
updateChunkMesh ref materials scene chunkWithMesh = void do

    VertexDataPropsData verts@{ terrain: Chunk chunk } <- pure (createTerrainGeometry chunkWithMesh.blocks)

    let index = chunk.index
    State state <- readRef ref
    case lookupChunk chunk.index state.terrain of
        Nothing -> pure unit
        Just chunkData -> disposeChunk chunkData

    standardMaterialMesh <- generateMesh index verts.standardMaterialBlocks materials.boxMat scene
    mesh <- pure { blocks: verts.terrain, standardMaterialMesh }
    liftEff $ writeRef ref $ State state {
        terrain = insertChunk mesh state.terrain
    }
