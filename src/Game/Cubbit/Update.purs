module Game.Cubbit.Update (update, pickBlock, requestPointerLock, exitPointerLock) where

import Control.Alt (void)
import Control.Alternative (pure, when)
import Control.Bind (bind)
import Control.Comonad.Store.Class (seeks)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF, Ref, modifyRef, newRef, readRef, writeRef)
import DOM (DOM)
import Data.Array (catMaybes, drop, take)
import Data.Foldable (for_)
import Data.Int (toNumber) as Int
import Data.Maybe (Maybe(Just, Nothing))
import Data.Nullable (Nullable, toNullable)
import Data.Ord (abs, min)
import Data.Show (show)
import Data.Unit (Unit, unit)
import Game.Cubbit.BlockIndex (BlockIndex, runBlockIndex)
import Game.Cubbit.Chunk (MeshLoadingState(MeshNotLoaded, MeshLoaded), disposeChunk)
import Game.Cubbit.ChunkIndex (chunkIndex, runChunkIndex)
import Game.Cubbit.ChunkMap (delete, filterNeighbors, getSortedChunks, size, slice)
import Game.Cubbit.MeshBuilder (createChunkMesh)
import Game.Cubbit.Terrain (Terrain(Terrain), globalPositionToChunkIndex, globalPositionToGlobalIndex, isSolidBlock, lookupBlockByVec, lookupChunk, lookupSolidBlockByVec)
import Game.Cubbit.Types (Effects, Mode(..), State(State), Materials, ForeachIndex, Options)
import Graphics.Babylon (BABYLON)
import Graphics.Babylon.AbstractMesh (abstractMeshToNode, getSkeleton, setRotation, setVisibility)
import Graphics.Babylon.AbstractMesh (setPosition) as AbstractMesh
import Graphics.Babylon.Camera (setPosition) as Camera
import Graphics.Babylon.Mesh (meshToAbstractMesh, setPosition)
import Graphics.Babylon.Node (getName)
import Graphics.Babylon.PickingInfo (getPickedPoint)
import Graphics.Babylon.Ray (createRayWithLength)
import Graphics.Babylon.Scene (pick, pickWithRay)
import Graphics.Babylon.ShadowGenerator (ShadowMap, setRenderList)
import Graphics.Babylon.Skeleton (beginAnimation)
import Graphics.Babylon.TargetCamera (TargetCamera, setTarget, targetCameraToCamera)
import Graphics.Babylon.Types (Mesh, Scene)
import Graphics.Babylon.Vector3 (createVector3, runVector3, subtract, length)
import Math (atan2, cos, max, pi, round, sin, sqrt)
import Prelude (negate, ($), (&&), (*), (+), (-), (/), (/=), (<), (<$>), (<>), (==), (||))

playAnimation :: forall eff. String -> Ref State -> Eff (Effects eff) Unit
playAnimation name ref = do
    State state <- readRef ref
    for_ state.playerMeshes \mesh -> void do
        skeleton <- getSkeleton mesh
        beginAnimation name true 1.0 pure skeleton

pickBlock :: forall e. Scene -> Mesh -> State -> Int -> Int -> Eff (dom :: DOM, ref :: REF, babylon :: BABYLON | e) (Maybe BlockIndex)
pickBlock scene cursor (State state) screenX screenY = do
    let predicate mesh = do
            let name = getName (abstractMeshToNode mesh)
            pure (name /= "cursor")

    pickingInfo <- pick screenX screenY predicate false scene

    case getPickedPoint pickingInfo of
        Nothing -> pure Nothing
        Just point -> do
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


update :: forall eff. Ref State -> Scene -> Materials -> ShadowMap -> Mesh -> TargetCamera -> Options -> Mesh -> Eff (Effects eff) Unit
update ref scene materials shadowMap cursor camera options skybox = do

        modifyRef ref \(State state) -> State state { totalFrames = state.totalFrames + 1 }

        State state@{ terrain: Terrain ter } <- readRef ref

        activeChunks <- slice 0 125 ter.map

        -- update camera position
        let cameraPosition = state.cameraPosition
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
            let cci = runChunkIndex cameraPositionChunkIndex
            neighbors <- filterNeighbors options.shadowDisplayRange cci.x cci.y cci.z ter.map
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

                    -- let ci = runChunkIndex index

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

            let rot =  negate if st.firstPersonView then (st.playerRotation + pi) else  st.cameraYaw

            let keyVector = {
                    x: if st.dKey then 1.0 else 0.0 - if st.aKey then 1.0 else 0.0,
                    z: if st.wKey then 1.0 else 0.0 - if st.sKey then 1.0 else 0.0
                }

            let rotatedKeyVector = {
                    x: cos rot * keyVector.x - sin rot * keyVector.z,
                    z: sin rot * keyVector.x + cos rot * keyVector.z
                }

            let stopped = rotatedKeyVector.x == 0.0 && rotatedKeyVector.z == 0.0

            let velocity = if stopped
                        then state.velocity {
                            x = state.velocity.x * 0.5,
                            z = state.velocity.z * 0.5
                        }
                        else let len = sqrt (rotatedKeyVector.x * rotatedKeyVector.x + rotatedKeyVector.z * rotatedKeyVector.z) in state.velocity {
                            x = rotatedKeyVector.x / len * speed,
                            z = rotatedKeyVector.z / len * speed
                        }

            -- playerRotation == 0   =>    -z direction
            let playerRotation' = if stopped || st.firstPersonView then st.playerRotation else (atan2 velocity.x velocity.z) - pi

            let playerPosition = {
                        x: state.position.x + velocity.x,
                        y: state.position.y + velocity.y,
                        z: state.position.z + velocity.z
                    }

            let animation' = if st.wKey || st.sKey || st.aKey || st.dKey then "Action" else "Stand"

            let globalIndex = runBlockIndex (globalPositionToGlobalIndex playerPosition.x playerPosition.y playerPosition.z)
            blockMaybe <- lookupBlockByVec playerPosition terrain

            let position' = case blockMaybe of
                                Just block | isSolidBlock block -> playerPosition {
                                    y = Int.toNumber (globalIndex.y) + 1.001
                                }
                                _ -> playerPosition

            let velocity' = case blockMaybe of
                                Just block | isSolidBlock block -> velocity { y = 0.0 }
                                _ -> velocity { y = velocity.y - 0.01 }


            -- camera view target
            let cameraSpeed = if st.firstPersonView then 0.5 else options.cameraTargetSpeed

            let eyeHeight = 1.4

            let thirdPersonCameraTargetX = position'.x
            let thirdPersonCameraTargetY = position'.y + eyeHeight
            let thirdPersonCameraTargetZ = position'.z

            let playerRotationTheta = negate st.playerRotation - pi * 0.5
            let firstPersonCameraTargetX = position'.x + cos playerRotationTheta * cos st.playerPitch
            let firstPersonCameraTargetY = position'.y + eyeHeight + sin st.playerPitch
            let firstPersonCameraTargetZ = position'.z + sin playerRotationTheta * cos st.playerPitch

            let cameraTargetX' = if st.firstPersonView then firstPersonCameraTargetX else thirdPersonCameraTargetX
            let cameraTargetY' = if st.firstPersonView then firstPersonCameraTargetY else thirdPersonCameraTargetY
            let cameraTargetZ' = if st.firstPersonView then firstPersonCameraTargetZ else thirdPersonCameraTargetZ

            let cameraTargetInterpolatedX' = st.viewReferencePoint.x + (cameraTargetX' - st.viewReferencePoint.x) * cameraSpeed
            let cameraTargetInterpolatedY' = st.viewReferencePoint.y + (cameraTargetY' - st.viewReferencePoint.y) * cameraSpeed
            let cameraTargetInterpolatedZ' = st.viewReferencePoint.z + (cameraTargetZ' - st.viewReferencePoint.z) * cameraSpeed

            let viewReferencePoint' = { x: cameraTargetInterpolatedX', y: cameraTargetInterpolatedY', z: cameraTargetInterpolatedZ' }

            -- camera position
            let theta = negate st.cameraYaw - pi * 0.5
            let thirdPersonCameraPositionX = position'.x + cos theta * cos st.cameraPitch * st.cameraRange
            let thirdPersonCameraPositionY = position'.y + sin st.cameraPitch * st.cameraRange
            let thirdPersonCameraPositionZ = position'.z + sin theta * cos st.cameraPitch * st.cameraRange

            let firstPersonCameraPositionX = position'.x
            let firstPersonCameraPositionY = position'.y + eyeHeight
            let firstPersonCameraPositionZ = position'.z

            let cameraPositionX = if st.firstPersonView then firstPersonCameraPositionX else thirdPersonCameraPositionX
            let cameraPositionY = if st.firstPersonView then firstPersonCameraPositionY else thirdPersonCameraPositionY
            let cameraPositionZ = if st.firstPersonView then firstPersonCameraPositionZ else thirdPersonCameraPositionZ

            let cameraPositionInterpolatedX = cameraPosition.x + (cameraPositionX - cameraPosition.x) * cameraSpeed
            let cameraPositionInterpolatedY = cameraPosition.y + (cameraPositionY - cameraPosition.y) * cameraSpeed
            let cameraPositionInterpolatedZ = cameraPosition.z + (cameraPositionZ - cameraPosition.z) * cameraSpeed

            -- final state

            let st' = st {
                        position = position',
                        velocity = velocity',

                        cameraPosition = { x:cameraPositionInterpolatedX, y: cameraPositionInterpolatedY, z: cameraPositionInterpolatedZ },
                        cameraYaw = st.cameraYaw + ((if st.qKey then 1.0 else 0.0) - (if st.eKey then 1.0 else 0.0)) * options.cameraRotationSpeed,
                        cameraPitch = max 0.1 (min (pi * 0.48) (st.cameraPitch + ((if st.rKey then 1.0 else 0.0) - (if st.fKey then 1.0 else 0.0)) * options.cameraRotationSpeed)),
                        cameraRange = max 3.0 (min 20.0 (st.cameraRange + ((if st.gKey then 1.0 else 0.0) - (if st.tKey then 1.0 else 0.0)) * options.cameraZoomSpeed)),
                        animation = animation',
                        viewReferencePoint = viewReferencePoint',
                        playerRotation = playerRotation'
                    }

            -- update states
            writeRef ref (State st')

            when (animation' /= st.animation) do
                playAnimation animation' ref

            playerRotationVector <- createVector3 0.0 playerRotation' 0.0
            position <- createVector3 st'.position.x st'.position.y st'.position.z
            for_ st.playerMeshes \mesh -> void do
                AbstractMesh.setPosition position mesh
                setRotation playerRotationVector mesh
                setVisibility (if st.firstPersonView then 0.0 else 1.0) mesh

            -- update camera

            cameraPosition' <- createVector3 cameraPositionInterpolatedX cameraPositionInterpolatedY cameraPositionInterpolatedZ

            cameraTarget' <- createVector3 cameraTargetInterpolatedX' cameraTargetInterpolatedY' cameraTargetInterpolatedZ'

            cameraDirection <- subtract cameraTarget' cameraPosition'
            cameraDirectionLength <- length cameraDirection
            cameraRay <- createRayWithLength cameraPosition' cameraDirection cameraDirectionLength
            let predicate mesh = do
                    let name = getName (abstractMeshToNode mesh)
                    pure (name == "terrain")
            picked <- pickWithRay cameraRay predicate true scene

            let pickedPoint = getPickedPoint picked

            let cameraPosition'' = case getPickedPoint picked of
                    Nothing -> cameraPosition'
                    Just point -> point
            Camera.setPosition cameraPosition'' (targetCameraToCamera camera)
            setTarget cameraTarget' camera

            skyboxRotationVector <- createVector3 0.0 (Int.toNumber st.totalFrames * 0.0001) 0.0
            setRotation skyboxRotationVector (meshToAbstractMesh skybox)

            pure unit

foreign import foreachBlocks :: forall eff. Int -> Int -> Int -> Int -> Nullable ForeachIndex -> (Int -> Int -> Int -> Eff eff Int) -> Eff eff ForeachIndex

foreign import setTextContent :: forall eff. String -> String -> Eff (dom :: DOM | eff) Unit

foreign import requestPointerLock :: ∀eff . ({ movementX :: Number, movementY :: Number } -> Eff (dom :: DOM | eff) Unit) -> Eff (dom :: DOM | eff) Unit -> Eff (dom :: DOM | eff) Unit

foreign import exitPointerLock :: ∀eff . Eff (dom :: DOM | eff) Unit
