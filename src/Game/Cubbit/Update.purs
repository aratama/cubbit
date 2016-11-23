module Game.Cubbit.Update (update, pickBlock) where

import Control.Alt (void)
import Control.Alternative (pure)
import Control.Bind (bind, when, (>>=))
import Control.Monad.Eff (Eff, forE)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Ref (REF, Ref, modifyRef, readRef, writeRef)
import Control.MonadPlus (guard)
import DOM (DOM)
import Data.Array (head, (..))
import Data.Array.ST (emptySTArray, pushSTArray, runSTArray)
import Data.Foldable (for_)
import Data.Int (toNumber) as Int
import Data.Maybe (Maybe(Just, Nothing), isNothing)
import Data.Ord (abs, min)
import Data.Show (show)
import Data.Unit (Unit, unit)
import Graphics.Babylon (BABYLON)
import Graphics.Babylon.AbstractMesh (moveWithCollisions)
import Graphics.Babylon.AbstractMesh (abstractMeshToNode, setCheckCollisions, getPosition, setPosition) as AbstractMesh
import Graphics.Babylon.Camera (getPosition) as Camera
import Game.Cubbit.BlockIndex (BlockIndex, runBlockIndex)
import Game.Cubbit.Chunk (Chunk(..))
import Game.Cubbit.ChunkIndex (chunkIndex, chunkIndexDistance, runChunkIndex)
import Game.Cubbit.MeshBuilder (createChunkMesh)
import Game.Cubbit.Terrain (Terrain, chunkCount, getChunkMap, globalPositionToChunkIndex, globalPositionToGlobalIndex, lookupBlockByVec, lookupChunk)
import Game.Cubbit.Types (Effects, Mode(..), State(State), Materials)
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
import Prelude (mod, ($), (+), (-), (/=), (<=), (<>), (==))

shadowMapSize :: Int
shadowMapSize = 4096

loadDistance :: Int
loadDistance = 4

unloadDistance :: Int
unloadDistance = 8

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
                        case lookupBlock' (p.x + 0.5) p.y p.z, lookupBlock' (p.x - 0.5) p.y p.z of
                            Just block, Nothing -> pure $ Just $ globalPositionToGlobalIndex (p.x - 0.5) p.y p.z
                            Nothing, Just block -> pure $ Just $ globalPositionToGlobalIndex (p.x + 0.5) p.y p.z
                            _, _ -> pure Nothing
                        else if minDelta == dy then do
                                case lookupBlock' p.x (p.y + 0.5) p.z, lookupBlock' p.x (p.y - 0.5) p.z of
                                    Just block, Nothing -> pure $ Just $ globalPositionToGlobalIndex p.x (p.y - 0.5) p.z
                                    Nothing, Just block -> pure $ Just $ globalPositionToGlobalIndex p.x (p.y + 0.5) p.z
                                    _, _ -> pure Nothing
                            else do
                                case lookupBlock' p.x p.y (p.z + 0.5), lookupBlock' p.x p.y (p.z - 0.5) of
                                    Just block, Nothing -> pure $ Just $ globalPositionToGlobalIndex p.x p.y (p.z - 0.5)
                                    Nothing, Just block -> pure $ Just $ globalPositionToGlobalIndex p.x p.y (p.z + 0.5)
                                    _, _ -> pure Nothing

                Remove -> if minDelta == dx then do
                        case lookupBlock' (p.x + 0.5) p.y p.z, lookupBlock' (p.x - 0.5) p.y p.z of
                            Just block, Nothing -> pure $ Just $ globalPositionToGlobalIndex (p.x + 0.5) p.y p.z
                            Nothing, Just block -> pure $ Just $ globalPositionToGlobalIndex (p.x - 0.5) p.y p.z
                            _, _ -> pure Nothing
                        else if minDelta == dy then do
                                case lookupBlock' p.x (p.y + 0.5) p.z, lookupBlock' p.x (p.y - 0.5) p.z of
                                    Just block, Nothing -> pure $ Just $ globalPositionToGlobalIndex p.x (p.y + 0.5) p.z
                                    Nothing, Just block -> pure $ Just $ globalPositionToGlobalIndex p.x (p.y - 0.5) p.z
                                    _, _ -> pure Nothing
                            else do
                                case lookupBlock' p.x p.y (p.z + 0.5), lookupBlock' p.x p.y (p.z - 0.5) of
                                    Just block, Nothing -> pure $ Just $ globalPositionToGlobalIndex p.x p.y (p.z + 0.5)
                                    Nothing, Just block -> pure $ Just $ globalPositionToGlobalIndex p.x p.y (p.z - 0.5)
                                    _, _ -> pure Nothing

                Move -> pure Nothing

    if getHit pickingInfo then pickup else pure Nothing

update :: forall eff. Ref State -> Scene -> Materials -> ShadowMap -> Mesh -> FreeCamera -> Eff (Effects eff) Unit
update ref scene materials shadowMap cursor camera = do

        modifyRef ref \(State state) -> State state { totalFrames = state.totalFrames + 1 }

        State state <- readRef ref

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


        when (mod state.totalFrames 10 == 0) do
            let shadowRange = 0
            let ci = runChunkIndex cameraPositionChunkIndex
            chunks <- runSTArray do
                list <- emptySTArray
                -- for_ state.playerMeshes \mesh -> pushSTArray list mesh
                forE (ci.x - shadowRange) (ci.x + shadowRange) \dx -> do
                    forE (ci.y - shadowRange) (ci.y + shadowRange) \dy -> do
                        forE (ci.z - shadowRange) (ci.z + shadowRange) \dz -> do
                            case lookupChunk (chunkIndex dx dy dz) state.terrain of
                                Just chunkData@{ standardMaterialMesh: Just mesh } -> void do
                                    pushSTArray list (meshToAbstractMesh mesh)
                                _ -> pure unit
                pure list

            setRenderList chunks shadowMap



        -- load chunk
        do
            let indices = do
                    let ci = runChunkIndex cameraPositionChunkIndex
                    x <- (ci.x - loadDistance) .. (ci.x + loadDistance)
                    y <- (ci.y - 1) .. (ci.y + 1)
                    z <- (ci.z - loadDistance) .. (ci.z + loadDistance)
                    guard (isNothing (lookupChunk (chunkIndex x y z) state.terrain))
                    pure (chunkIndex x y z)

            case head indices of
                Nothing -> pure unit
                Just index -> do
                    createChunkMesh ref materials scene index
                    log $ "load chunk: " <> show index
                    log $ "total chunks:" <> show (chunkCount state.terrain + 1)


        -- set collesion

        do
            State st <- readRef ref
            for_ (getChunkMap st.terrain) \(dat@{ blocks: Chunk chunk }) -> do
                let r = chunkIndexDistance chunk.index cameraPositionChunkIndex
                let enabled = r <= collesionEnabledRange
                case dat.standardMaterialMesh of
                    Nothing -> pure unit
                    Just mesh -> AbstractMesh.setCheckCollisions enabled (meshToAbstractMesh mesh)



        do
            State st@{ terrain } <- readRef ref

            let next = {
                        x: state.position.x + state.velocity.x,
                        y: state.position.y + state.velocity.y,
                        z: state.position.z + state.velocity.z
                    }
            let globalIndex = runBlockIndex (globalPositionToGlobalIndex next.x next.y next.z)
            let st' = case lookupBlockByVec next terrain of
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



            pure unit
            {-}

            let velocity = {
                        x: if state.position.x == currentPosition.x then 0.0 else state.velocity.x,
                        y: (if state.position.y == currentPosition.y then 0.0 else state.velocity.y) - 0.01,
                        z: if state.position.z == currentPosition.z then 0.0 else state.velocity.z
                    }
            modifyRef ref \(State state) -> State state {
                position = currentPosition,
                velocity = velocity
            }
-}

        do
            pos <- Camera.getPosition (freeCameraToCamera camera) >>= runVector3
            cameraRot <- getRotation (freeCameraToTargetCamera camera) >>= runVector3
            -- renderMiniMap state.terrain pos cameraRot minimap
            pure unit
