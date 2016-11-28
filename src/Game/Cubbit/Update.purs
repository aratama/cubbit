module Game.Cubbit.Update (update, pickBlock) where

import Control.Alt (void)
import Control.Alternative (pure)
import Control.Bind (bind, when, (>>=))
import Control.Comonad.Store (store)
import Control.Comonad.Store.Class (seeks)
import Control.Monad.Eff (Eff, forE)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Ref (REF, Ref, modifyRef, newRef, readRef, writeRef)
import Control.Monad.List.Trans (zipWith)
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Control.MonadPlus (guard)
import DOM (DOM)
import Data.Array (catMaybes, head)
import Data.Array.ST (emptySTArray, pushSTArray, runSTArray)
import Data.Foldable (for_)
import Data.Int (toNumber) as Int
import Data.List (List(..), elem, fromFoldable, notElem, nub, (:), (..))
import Data.Maybe (Maybe(..), isNothing)
import Data.Nullable (Nullable, toNullable)
import Data.Ord (abs, min)
import Data.Show (show)
import Data.Unit (Unit, unit)
import Game.Cubbit.BlockIndex (BlockIndex, runBlockIndex)
import Game.Cubbit.Chunk (Chunk(..), ChunkWithMesh, MeshLoadingState(..))
import Game.Cubbit.ChunkIndex (chunkIndex, chunkIndexDistance, runChunkIndex)
import Game.Cubbit.ChunkMap (delete, peekAt, size, slice, sort)
import Game.Cubbit.Constants (loadDistance, unloadDistance)
import Game.Cubbit.MeshBuilder (createChunkMesh, loadDefaultChunk)
import Game.Cubbit.Terrain (Terrain(..), chunkCount, disposeChunk, getChunkMap, globalPositionToChunkIndex, globalPositionToGlobalIndex, lookupBlockByVec, lookupChunk)
import Game.Cubbit.Types (Effects, Mode(..), State(State), Materials, ForeachIndex)
import Graphics.Babylon (BABYLON)
import Graphics.Babylon.AbstractMesh (moveWithCollisions, setIsPickable)
import Graphics.Babylon.AbstractMesh (abstractMeshToNode, setCheckCollisions, getPosition, setPosition) as AbstractMesh
import Graphics.Babylon.Camera (getPosition) as Camera
import Graphics.Babylon.FreeCamera (FreeCamera, freeCameraToCamera, freeCameraToTargetCamera)
import Graphics.Babylon.Mesh (meshToAbstractMesh, setPosition)
import Graphics.Babylon.Node (getName)
import Graphics.Babylon.PickingInfo (getHit, getPickedPoint)
import Graphics.Babylon.Scene (pick)
import Graphics.Babylon.ShadowGenerator (ShadowMap, setRenderList)
import Graphics.Babylon.TargetCamera (getRotation)
import Graphics.Babylon.Types (AbstractMesh, Mesh, Scene)
import Graphics.Babylon.Vector3 (createVector3, runVector3, toVector3)
import Math (round)
import Prelude (mod, ($), (+), (-), (/=), (<=), (<>), (==), (#), (<), (<=), negate, (&&), (<$>))

shadowMapSize :: Int
shadowMapSize = 4096

skyBoxRenderingGruop :: Int
skyBoxRenderingGruop = 0

terrainRenderingGroup :: Int
terrainRenderingGroup = 1

collesionEnabledRange :: Int
collesionEnabledRange = 3

enableWaterMaterial :: Boolean
enableWaterMaterial = false

pickBlock :: forall e. Scene -> Mesh -> State -> Int -> Int -> Eff (dom :: DOM, ref :: REF, babylon :: BABYLON | e) (Maybe BlockIndex)
pickBlock scene cursor (State state) screenX screenY = do
    let predicate mesh = do
            let name = getName (AbstractMesh.abstractMeshToNode mesh)
            pure (name /= "cursor")

    pickingInfo <- pick screenX screenY predicate false scene

    let pickup = do
            let point = getPickedPoint pickingInfo
            p <- runVector3 point
            let dx = abs (p.x - round p.x)
            let dy = abs (p.y - round p.y)
            let dz = abs (p.z - round p.z)
            let minDelta = min dx (min dy dz)
            let lookupBlock' x y z = lookupBlockByVec { x, y, z } state.terrain

            let putCursor bi = do
                    let rbi = runBlockIndex bi
                    r <- createVector3 (Int.toNumber rbi.x + 0.5) (Int.toNumber rbi.y + 0.5) (Int.toNumber rbi.z + 0.5)
                    setPosition r cursor

            case state.mode of
                Put -> if minDelta == dx then do
                        l <- lookupBlock' (p.x + 0.5) p.y p.z
                        r <- lookupBlock' (p.x - 0.5) p.y p.z
                        case l, r of
                            Just block, Nothing -> pure $ Just $ globalPositionToGlobalIndex (p.x - 0.5) p.y p.z
                            Nothing, Just block -> pure $ Just $ globalPositionToGlobalIndex (p.x + 0.5) p.y p.z
                            _, _ -> pure Nothing
                        else if minDelta == dy then do
                                m <- lookupBlock' p.x (p.y + 0.5) p.z
                                n <- lookupBlock' p.x (p.y - 0.5) p.z
                                case m, n of
                                    Just block, Nothing -> pure $ Just $ globalPositionToGlobalIndex p.x (p.y - 0.5) p.z
                                    Nothing, Just block -> pure $ Just $ globalPositionToGlobalIndex p.x (p.y + 0.5) p.z
                                    _, _ -> pure Nothing
                            else do
                                x <- lookupBlock' p.x p.y (p.z + 0.5)
                                y <- lookupBlock' p.x p.y (p.z - 0.5)
                                case x, y of
                                    Just block, Nothing -> pure $ Just $ globalPositionToGlobalIndex p.x p.y (p.z - 0.5)
                                    Nothing, Just block -> pure $ Just $ globalPositionToGlobalIndex p.x p.y (p.z + 0.5)
                                    _, _ -> pure Nothing

                Remove -> if minDelta == dx then do
                        l <- lookupBlock' (p.x + 0.5) p.y p.z
                        r <- lookupBlock' (p.x - 0.5) p.y p.z
                        case l, r of
                            Just block, Nothing -> pure $ Just $ globalPositionToGlobalIndex (p.x + 0.5) p.y p.z
                            Nothing, Just block -> pure $ Just $ globalPositionToGlobalIndex (p.x - 0.5) p.y p.z
                            _, _ -> pure Nothing
                        else if minDelta == dy then do
                                m <- lookupBlock' p.x (p.y + 0.5) p.z
                                n <- lookupBlock' p.x (p.y - 0.5) p.z
                                case m, n of
                                    Just block, Nothing -> pure $ Just $ globalPositionToGlobalIndex p.x (p.y + 0.5) p.z
                                    Nothing, Just block -> pure $ Just $ globalPositionToGlobalIndex p.x (p.y - 0.5) p.z
                                    _, _ -> pure Nothing
                            else do
                                x <- lookupBlock' p.x p.y (p.z + 0.5)
                                y <- lookupBlock' p.x p.y (p.z - 0.5)
                                case x, y of
                                    Just block, Nothing -> pure $ Just $ globalPositionToGlobalIndex p.x p.y (p.z + 0.5)
                                    Nothing, Just block -> pure $ Just $ globalPositionToGlobalIndex p.x p.y (p.z - 0.5)
                                    _, _ -> pure Nothing

                Move -> pure Nothing

    if getHit pickingInfo then pickup else pure Nothing

update :: forall eff. Ref State -> Scene -> Materials -> ShadowMap -> Mesh -> FreeCamera -> Eff (Effects eff) Unit
update ref scene materials shadowMap cursor camera = do

        modifyRef ref \(State state) -> State state { totalFrames = state.totalFrames + 1 }

        State state@{ terrain: Terrain ter } <- readRef ref

        activeChunks <- slice 0 125 ter.map

        -- update camera position
        cameraPosition <- Camera.getPosition (freeCameraToCamera camera) >>= runVector3
        let cameraPositionChunkIndex = globalPositionToChunkIndex cameraPosition.x cameraPosition.y cameraPosition.z

        -- picking
        do
            case state.mode of
                Move -> pure unit
                _ -> do
                    picked <- pickBlock scene cursor (State state) state.mousePosition.x state.mousePosition.y
                    case picked of
                        Nothing -> pure unit
                        Just bi -> do
                            let rbi = runBlockIndex bi
                            r <- createVector3 (Int.toNumber rbi.x + 0.5) (Int.toNumber rbi.y + 0.5) (Int.toNumber rbi.z + 0.5)
                            setPosition r cursor

        -- update shadow rendering list
        let shdowRenderingMeshes = (catMaybes ( (\chunk -> case chunk.standardMaterialMesh of
                    MeshLoaded mesh -> Just (meshToAbstractMesh mesh)
                    _ -> Nothing
                ) <$> activeChunks))
        setRenderList shdowRenderingMeshes shadowMap



        -- load chunk
        do
            let costLimit = 100
            costRef <- newRef 0

            let ci = runChunkIndex cameraPositionChunkIndex

            let loadAndGenerateChunk index = do

                    createChunkMesh ref materials scene index

                    State st <- readRef ref
                    size <- chunkCount st.terrain
                    log $ "total chunks:" <> show (size + 1)
                    -- log $ "load chunk: " <> show index

            nextIndex <- foreachBlocks loadDistance ci.x ci.y ci.z state.updateIndex \x y z -> do

                let index = chunkIndex x y z
                chunkMaybe <- lookupChunk index state.terrain
                case chunkMaybe of
                    Just chunkWithMaybe -> do
                        case chunkWithMaybe.standardMaterialMesh of
                            MeshNotLoaded -> do
                                loadAndGenerateChunk index
                                pure 100
                            _ -> pure 1
                    Nothing -> do
                        loadAndGenerateChunk index
                        pure 100


            modifyRef ref \(State st) -> State st {
                updateIndex = toNullable (Just nextIndex)
            }

        -- unload sequence
        do

            let ci = runChunkIndex cameraPositionChunkIndex
            State st@{ terrain: Terrain terrain }<- readRef ref
            sort ci.x ci.y ci.z terrain.map

            tailRecM (\i -> if 100 < i then pure (Done unit) else do
                s <- size terrain.map
                chunkWithMeshMaybe <- peekAt (s - 1) terrain.map
                case chunkWithMeshMaybe of
                    Nothing -> pure (Loop (i + 1))
                    Just chunkWithMesh@{ blocks: Chunk chunk } -> do
                        let distance = chunkIndexDistance cameraPositionChunkIndex chunk.index
                        if unloadDistance <= distance
                            then do
                                disposeChunk chunkWithMesh
                                delete chunk.index terrain.map
                                let ci = runChunkIndex chunk.index
                                log ("unload: " <> show ci.x <> ", " <> show ci.y <> ", " <> show ci.z )
                                pure (Loop (i + 20))
                            else pure (Loop (i + 1))
            ) 0



        do
            State st@{ terrain } <- readRef ref

            let next = {
                        x: state.position.x + state.velocity.x,
                        y: state.position.y + state.velocity.y,
                        z: state.position.z + state.velocity.z
                    }
            let globalIndex = runBlockIndex (globalPositionToGlobalIndex next.x next.y next.z)
            blockMaybe <- lookupBlockByVec next terrain
            let st' = case blockMaybe of
                            Nothing -> st {
                                position = next,
                                velocity = st.velocity { y = state.velocity.y - 0.01 }
                            }
                            Just _ -> st {
                                position = {
                                    x: st.position.x,
                                    y: Int.toNumber (globalIndex.y + 1),
                                    z: st.position.z
                                },
                                velocity = { x: 0.0, y: 0.0, z: 0.0 }
                            }

            writeRef ref (State st')

            for_ st.playerMeshes \mesh -> void do
                position <- createVector3 st'.position.x (st'.position.y + 0.501) st'.position.z
                AbstractMesh.setPosition position mesh







foreign import foreachBlocks :: forall eff. Int -> Int -> Int -> Int -> Nullable ForeachIndex -> (Int -> Int -> Int -> Eff eff Int) -> Eff eff ForeachIndex
