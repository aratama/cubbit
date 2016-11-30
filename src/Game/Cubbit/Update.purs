module Game.Cubbit.Update (update, pickBlock) where

import Control.Alt (void)
import Control.Alternative (pure, when)
import Control.Bind (bind, (>>=))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, logShow)
import Control.Monad.Eff.Ref (REF, Ref, modifyRef, newRef, readRef, writeRef)
import Control.Monad.Rec.Class (Step(..), tailRecM)
import DOM (DOM)
import Data.Array (catMaybes, drop, last, length, take)
import Data.Array (slice) as Array
import Data.Foldable (for_)
import Data.Int (toNumber) as Int
import Data.Maybe (Maybe(Just, Nothing))
import Data.Nullable (Nullable, toNullable)
import Data.Ord (abs, min)
import Data.Show (show)
import Data.Unit (Unit, unit)
import Game.Cubbit.BlockIndex (BlockIndex, runBlockIndex)
import Game.Cubbit.BlockType (airBlock, bushBlock)
import Game.Cubbit.Chunk (Chunk(Chunk), MeshLoadingState(..), disposeChunk)
import Game.Cubbit.ChunkIndex (chunkIndex, chunkIndexDistance, runChunkIndex)
import Game.Cubbit.ChunkMap (delete, peekAt, size, slice, sort, filterNeighbors, getSortedChunks)
import Game.Cubbit.MeshBuilder (createChunkMesh)
import Game.Cubbit.Terrain (Terrain(Terrain), isSolidBlock, chunkCount, globalPositionToChunkIndex, globalPositionToGlobalIndex, lookupSolidBlockByVec, lookupBlockByVec, lookupChunk)
import Game.Cubbit.Types (Effects, Mode(..), State(State), Materials, ForeachIndex, Options)
import Graphics.Babylon (BABYLON)
import Graphics.Babylon.AbstractMesh (getSkeleton, setRotation)
import Graphics.Babylon.AbstractMesh (abstractMeshToNode, setPosition) as AbstractMesh
import Graphics.Babylon.Camera (getPosition) as Camera
import Graphics.Babylon.FreeCamera (FreeCamera, freeCameraToCamera, freeCameraToTargetCamera)
import Graphics.Babylon.Mesh (meshToAbstractMesh, setPosition)
import Graphics.Babylon.Node (getName)
import Graphics.Babylon.PickingInfo (getHit, getPickedPoint)
import Graphics.Babylon.Scene (pick)
import Graphics.Babylon.ShadowGenerator (ShadowMap, setRenderList)
import Graphics.Babylon.Skeleton (beginAnimation)
import Graphics.Babylon.TargetCamera (getRotation, setTarget, getTarget)
import Graphics.Babylon.Types (Mesh, Scene)
import Graphics.Babylon.Vector3 (createVector3, runVector3)
import Math (atan2, cos, round, sin, sqrt)
import Prelude (($), (+), (-), (/=), (<), (<$>), (<=), (<>), (==), (&&), negate, (>>=), (*), (/), (||))

playAnimation :: forall eff. String -> Ref State -> Eff (Effects eff) Unit
playAnimation name ref = do
    State state <- readRef ref
    for_ state.playerMeshes \mesh -> void do
        skeleton <- getSkeleton mesh
        beginAnimation name true 1.0 pure skeleton

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
            let lookupBlock' x y z = lookupSolidBlockByVec { x, y, z } state.terrain

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

update :: forall eff. Ref State -> Scene -> Materials -> ShadowMap -> Mesh -> FreeCamera -> Options -> Eff (Effects eff) Unit
update ref scene materials shadowMap cursor camera options = do

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

                            setTextContent "cursor-position" (show rbi.x <> ", " <> show rbi.y <> ", " <> show rbi.z)

        -- update shadow rendering list
        do
            let ci = runChunkIndex cameraPositionChunkIndex
            neighbors <- filterNeighbors options.shadowDisplayRange ci.x ci.y ci.z ter.map
            let meshes =  catMaybes ((\chunk -> case chunk.standardMaterialMesh of
                        MeshLoaded mesh -> Just (meshToAbstractMesh mesh)
                        _ -> Nothing
                    ) <$> neighbors)
            setRenderList (meshes <> state.playerMeshes) shadowMap


        -- load chunk
        do
            let costLimit = 100
            costRef <- newRef 0

            let ci = runChunkIndex cameraPositionChunkIndex

            let loadAndGenerateChunk index = do

                    let ci = runChunkIndex index

                    createChunkMesh ref materials scene index

                    --State st <- readRef ref
                    --size <- chunkCount st.terrain
                    --log $ "load chunk: " <> show ci.x <> "," <> show ci.y <> ", " <> show ci.z
                    --log $ "total chunks:" <> show (size + 1)


            nextIndex <- foreachBlocks options.loadDistance ci.x ci.y ci.z state.updateIndex \x y z -> do

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
            --sort ci.x ci.y ci.z terrain.map

            loadedChunkCount <- size terrain.map
            when (options.maximumLoadedChunks < loadedChunkCount) do
                sorted <- getSortedChunks ci.x ci.y ci.z terrain.map
                let sliced = drop options.maximumLoadedChunks sorted
                for_ (take options.chunkUnloadSpeed sliced) \chunkWithMesh -> do
                    disposeChunk chunkWithMesh
                    delete chunkWithMesh.index terrain.map
                    --let ci = runChunkIndex chunkWithMesh.index
                    --log ("unload: " <> show ci.x <> ", " <> show ci.y <> ", " <> show ci.z )

        do
            State st@{ terrain } <- readRef ref

            let speed = options.moveSpeed
            cameraRotationVector <- getRotation (freeCameraToTargetCamera camera)
            cameraRot <- runVector3 cameraRotationVector
            let vt = if st.wKey then 1.0 else 0.0 - if st.sKey then 1.0 else 0.0
            let ht = if st.dKey then 1.0 else 0.0 - if st.aKey then 1.0 else 0.0
            let vdx = sin cameraRot.y * vt
            let vdz = cos cameraRot.y * vt
            let hdx = sin (cameraRot.y + 3.141592 * 0.5) * ht
            let hdz = cos (cameraRot.y + 3.141592 * 0.5) * ht
            let dx = vdx + hdx
            let dz = vdz + hdz
            let moveAngle = atan2 dx dz
            let velocity = if dx == 0.0 && dz == 0.0
                        then state.velocity {
                            x = state.velocity.x * 0.5,
                            z = state.velocity.z * 0.5
                        }
                        else let len = sqrt (dx * dx + dz * dz) in state.velocity {
                            x = dx / len * speed,
                            z = dz / len * speed
                        }


            let next = {
                        x: state.position.x + velocity.x,
                        y: state.position.y + velocity.y,
                        z: state.position.z + velocity.z
                    }
            let globalIndex = runBlockIndex (globalPositionToGlobalIndex next.x next.y next.z)
            blockMaybe <- lookupBlockByVec next terrain

            let nextAnimation = if st.wKey || st.sKey || st.aKey || st.dKey then "Action" else "Stand"


            let onEmpty = st {
                        position = next,
                        velocity = velocity { y = velocity.y - 0.01 },
                        yaw = if dx == 0.0 && dz == 0.0 then st.yaw else moveAngle + 3.1415,
                        animation = nextAnimation
                    }
            let onGround = st {
                        position = {
                            x: next.x,
                            y: Int.toNumber (globalIndex.y) + 1.001,
                            z: next.z
                        },
                        velocity = velocity { y = 0.0 },
                        yaw = if dx == 0.0 && dz == 0.0 then st.yaw else moveAngle + 3.1415,
                        animation = nextAnimation
                    }

            let st' = case blockMaybe of
                            Just block | isSolidBlock block -> onGround
                            _ -> onEmpty

            writeRef ref (State st')

            -- update state
            when (nextAnimation /= st.animation) do
                playAnimation nextAnimation ref



            rotVector <- createVector3 0.0 st'.yaw 0.0

            logShow st'.position.x

            position <- createVector3 st'.position.x st'.position.y st'.position.z
            for_ st.playerMeshes \mesh -> void do
                AbstractMesh.setPosition position mesh
                setRotation rotVector mesh


            currentCameraTarget <- getTarget (freeCameraToTargetCamera camera) >>= runVector3
            let cx = currentCameraTarget.x + (st'.position.x - currentCameraTarget.x) * options.cameraTargetSpeed
            let cy = currentCameraTarget.y + (st'.position.y - currentCameraTarget.y) * options.cameraTargetSpeed
            let cz = currentCameraTarget.z + (st'.position.z - currentCameraTarget.z) * options.cameraTargetSpeed
            nextCameraTarget <- createVector3 cx cy cz
            setTarget nextCameraTarget (freeCameraToTargetCamera camera)


foreign import foreachBlocks :: forall eff. Int -> Int -> Int -> Int -> Nullable ForeachIndex -> (Int -> Int -> Int -> Eff eff Int) -> Eff eff ForeachIndex


foreign import setTextContent :: forall eff. String -> String -> Eff (dom :: DOM | eff) Unit
