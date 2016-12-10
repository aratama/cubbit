module Game.Cubbit.Update (update) where

import Control.Alt (void)
import Control.Alternative (pure, when)
import Control.Bind (bind)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (logShow)
import Control.Monad.Eff.Ref (Ref, modifyRef, newRef, readRef, writeRef)
import DOM (DOM)
import Data.Array (catMaybes, drop, take)
import Data.Foldable (for_)
import Data.Int (toNumber) as Int
import Data.Maybe (Maybe(Just, Nothing))
import Data.Nullable (Nullable, toNullable)
import Data.Ord (min, max)
import Data.Unit (Unit, unit)
import Data.Void (Void)
import Game.Cubbit.BlockIndex (runBlockIndex)
import Game.Cubbit.Chunk (MeshLoadingState(MeshNotLoaded, MeshLoaded), disposeChunk)
import Game.Cubbit.ChunkIndex (chunkIndex, runChunkIndex)
import Game.Cubbit.ChunkMap (delete, filterNeighbors, getSortedChunks, size)
import Game.Cubbit.Control (playAnimation, pickBlock)
import Game.Cubbit.Hud (Query(SetCursorPosition), queryToHud)
import Game.Cubbit.MeshBuilder (createChunkMesh)
import Game.Cubbit.Terrain (Terrain(Terrain), globalPositionToChunkIndex, globalPositionToGlobalIndex, isSolidBlock, lookupBlockByVec, lookupChunk)
import Game.Cubbit.Types (Effects, ForeachIndex, Materials, Mode(Move, Remove, Put), State(State))
import Game.Cubbit.Option (Options)
import Graphics.Babylon.AbstractMesh (abstractMeshToNode, setIsVisible, setRotation, setVisibility)
import Graphics.Babylon.AbstractMesh (setPosition) as AbstractMesh
import Graphics.Babylon.Camera (setPosition) as Camera
import Graphics.Babylon.Engine (getDeltaTime)
import Graphics.Babylon.Mesh (meshToAbstractMesh, setPosition)
import Graphics.Babylon.Node (getName)
import Graphics.Babylon.PickingInfo (getPickedPoint)
import Graphics.Babylon.Ray (createRayWithLength)
import Graphics.Babylon.Scene (pickWithRay)
import Graphics.Babylon.ShadowGenerator (setRenderList)
import Graphics.Babylon.TargetCamera (setTarget, targetCameraToCamera)
import Graphics.Babylon.Types (Engine, Mesh, Scene, ShadowMap, TargetCamera)
import Graphics.Babylon.Vector3 (createVector3, length, subtract)
import Halogen (HalogenIO)
import Math (atan2, cos, pi, sin, sqrt)
import Prelude (negate, (&&), (*), (+), (-), (/), (/=), (<), (<$>), (<>), (==), (||))

update :: forall eff. Ref State
                   -> Engine
                   -> Scene
                   -> Materials
                   -> ShadowMap
                   -> Mesh
                   -> TargetCamera
                   -> Options
                   -> Mesh
                   -> HalogenIO Query Void (Aff (Effects eff))
                   -> Eff (Effects eff) Unit
update ref engine scene materials shadowMap cursor camera options skybox driver = do

        State state@{ terrain: Terrain terrain } <- readRef ref

        -- calculate next state
        do
            -- calculate player velocity
            deltaTime <- getDeltaTime engine
            let speed = options.moveSpeed * deltaTime

            let rot =  negate if state.firstPersonView then (state.playerRotation + pi) else  state.cameraYaw

            let keyVector = {
                    x: if state.dKey then 1.0 else 0.0 - if state.aKey then 1.0 else 0.0,
                    z: if state.wKey then 1.0 else 0.0 - if state.sKey then 1.0 else 0.0
                }

            let rotatedKeyVector = {
                    x: cos rot * keyVector.x - sin rot * keyVector.z,
                    z: sin rot * keyVector.x + cos rot * keyVector.z
                }

            let stopped = rotatedKeyVector.x == 0.0 && rotatedKeyVector.z == 0.0

            let position = state.position
            footHoldBlockMaybe <- lookupBlockByVec { x: position.x, y: position.y - 0.01, z: position.z } state.terrain

            let isLanding = case footHoldBlockMaybe of
                    Just block | isSolidBlock block -> true
                    _ -> false

            let jumpVelocity = if isLanding && state.spaceKey && state.landing == 0 then options.jumpVelocity else 0.0

            let velocity = if 0 < state.landing then { x: 0.0, y: 0.0, z: 0.0 } else if stopped
                        then state.velocity {
                            x = state.velocity.x * 0.5,
                            y = state.velocity.y + jumpVelocity,
                            z = state.velocity.z * 0.5
                        }
                        else let len = sqrt (rotatedKeyVector.x * rotatedKeyVector.x + rotatedKeyVector.z * rotatedKeyVector.z) in state.velocity {
                            x = rotatedKeyVector.x / len * speed,
                            y = state.velocity.y + jumpVelocity,
                            z = rotatedKeyVector.z / len * speed
                        }

            -- playerRotation == 0   =>    -z direction
            let playerRotation' = if 0 < state.landing
                    then state.playerRotation
                    else if stopped || state.firstPersonView
                        then state.playerRotation
                        else (atan2 velocity.x velocity.z) - pi

            let playerPosition = {
                        x: state.position.x + velocity.x,
                        y: state.position.y + velocity.y,
                        z: state.position.z + velocity.z
                    }



            let animation' = if 0 < state.landing
                    then "land"
                    else if isLanding
                        then (if state.wKey || state.sKey || state.aKey || state.dKey then "run" else "idle")
                        else "jump"

            let globalIndex = runBlockIndex (globalPositionToGlobalIndex playerPosition.x playerPosition.y playerPosition.z)
            blockMaybe <- lookupBlockByVec playerPosition (Terrain terrain)

            let position' = case blockMaybe of
                                Just block | isSolidBlock block -> playerPosition {
                                    y = Int.toNumber (globalIndex.y) + 1.001
                                }
                                _ -> playerPosition

            let velocity' = case blockMaybe of
                                Just block | isSolidBlock block -> velocity { y = 0.0 }
                                _ -> velocity { y = velocity.y + options.gravity * deltaTime }




            footHoldBlockMaybe' <- lookupBlockByVec { x: position'.x, y: position'.y - 0.01, z: position'.z } state.terrain
            let isLanding' = case footHoldBlockMaybe' of
                    Just block | isSolidBlock block -> true
                    _ -> false

            let landingCount = if isLanding' && state.velocity.y < options.landingVelocityLimit then options.landingDuration else state.landing

            -- camera view target
            let cameraSpeed = if state.firstPersonView then 0.5 else options.cameraTargetSpeed

            let eyeHeight = options.eyeHeight

            let thirdPersonCameraTargetX = position'.x
            let thirdPersonCameraTargetY = position'.y + eyeHeight
            let thirdPersonCameraTargetZ = position'.z

            let playerRotationTheta = negate state.playerRotation - pi * 0.5
            let firstPersonCameraTargetX = position'.x + cos playerRotationTheta * cos state.playerPitch
            let firstPersonCameraTargetY = position'.y + eyeHeight + sin state.playerPitch
            let firstPersonCameraTargetZ = position'.z + sin playerRotationTheta * cos state.playerPitch

            let cameraTargetX' = if state.firstPersonView then firstPersonCameraTargetX else thirdPersonCameraTargetX
            let cameraTargetY' = if state.firstPersonView then firstPersonCameraTargetY else thirdPersonCameraTargetY
            let cameraTargetZ' = if state.firstPersonView then firstPersonCameraTargetZ else thirdPersonCameraTargetZ

            let cameraTargetInterpolatedX' = state.cameraTarget.x + (cameraTargetX' - state.cameraTarget.x) * cameraSpeed
            let cameraTargetInterpolatedY' = state.cameraTarget.y + (cameraTargetY' - state.cameraTarget.y) * cameraSpeed
            let cameraTargetInterpolatedZ' = state.cameraTarget.z + (cameraTargetZ' - state.cameraTarget.z) * cameraSpeed

            let cameraTarget' = { x: cameraTargetInterpolatedX', y: cameraTargetInterpolatedY', z: cameraTargetInterpolatedZ' }

            -- camera position
            let cameraPosition = state.cameraPosition
            let cameraPositionChunkIndex = globalPositionToChunkIndex cameraPosition.x cameraPosition.y cameraPosition.z

            let theta = negate state.cameraYaw - pi * 0.5
            let thirdPersonCameraPositionX = position'.x + cos theta * cos state.cameraPitch * state.cameraRange
            let thirdPersonCameraPositionY = position'.y + eyeHeight + sin state.cameraPitch * state.cameraRange
            let thirdPersonCameraPositionZ = position'.z + sin theta * cos state.cameraPitch * state.cameraRange

            let firstPersonCameraPositionX = position'.x
            let firstPersonCameraPositionY = position'.y + eyeHeight
            let firstPersonCameraPositionZ = position'.z

            let cameraPositionX = if state.firstPersonView then firstPersonCameraPositionX else thirdPersonCameraPositionX
            let cameraPositionY = if state.firstPersonView then firstPersonCameraPositionY else thirdPersonCameraPositionY
            let cameraPositionZ = if state.firstPersonView then firstPersonCameraPositionZ else thirdPersonCameraPositionZ

            let cameraPositionInterpolatedX = cameraPosition.x + (cameraPositionX - cameraPosition.x) * cameraSpeed
            let cameraPositionInterpolatedY = cameraPosition.y + (cameraPositionY - cameraPosition.y) * cameraSpeed
            let cameraPositionInterpolatedZ = cameraPosition.z + (cameraPositionZ - cameraPosition.z) * cameraSpeed

            -- final state

            let state' = state {
                        position = position',
                        velocity = velocity',

                        cameraTarget = cameraTarget',
                        cameraPosition = { x:cameraPositionInterpolatedX, y: cameraPositionInterpolatedY, z: cameraPositionInterpolatedZ },
                        cameraYaw = state.cameraYaw + ((if state.qKey then 1.0 else 0.0) - (if state.eKey then 1.0 else 0.0)) * options.cameraRotationSpeed,
                        cameraPitch = max 0.1 (min (pi * 0.48) (state.cameraPitch + ((if state.rKey then 1.0 else 0.0) - (if state.fKey then 1.0 else 0.0)) * options.cameraRotationSpeed)),
                        cameraRange = max options.cameraMinimumRange (min options.cameraMaximumRange (state.cameraRange + ((if state.gKey then 1.0 else 0.0) - (if state.tKey then 1.0 else 0.0)) * options.cameraZoomSpeed)),

                        animation = animation',
                        playerRotation = playerRotation',

                        totalFrames = state.totalFrames + 1,

                        skyboxRotation = state.skyboxRotation + options.skyboxRotationSpeed * deltaTime,

                        landing = max 0 (landingCount - 1)
                    }

            -- update states
            writeRef ref (State state')

            when (animation' /= state.animation) do
                playAnimation animation' ref

            playerRotationVector <- createVector3 0.0 playerRotation' 0.0
            positionVector <- createVector3 state'.position.x state'.position.y state'.position.z
            for_ state.playerMeshes \mesh -> void do
                AbstractMesh.setPosition positionVector mesh
                setRotation playerRotationVector mesh
                setVisibility (if state.firstPersonView then 0.0 else 1.0) mesh

            -- update camera

            cameraPositionVector <- createVector3 cameraPositionInterpolatedX cameraPositionInterpolatedY cameraPositionInterpolatedZ

            cameraTargetVector <- createVector3 cameraTargetInterpolatedX' cameraTargetInterpolatedY' cameraTargetInterpolatedZ'

            cameraDirection <- subtract cameraTargetVector cameraPositionVector
            cameraDirectionLength <- length cameraDirection
            cameraRay <- createRayWithLength cameraPositionVector cameraDirection cameraDirectionLength
            let predicate mesh = do
                    let name = getName (abstractMeshToNode mesh)
                    pure (name == "terrain")
            picked <- pickWithRay cameraRay predicate true scene

            let pickedPoint = getPickedPoint picked

            let cameraPosition'' = case getPickedPoint picked of
                    Nothing -> cameraPositionVector
                    Just point -> point
            Camera.setPosition cameraPosition'' (targetCameraToCamera camera)
            setTarget cameraTargetVector camera

            skyboxRotationVector <- createVector3 0.0 state'.skyboxRotation 0.0
            setRotation skyboxRotationVector (meshToAbstractMesh skybox)


            -- load chunks
            do
                let costLimit = 100
                costRef <- newRef 0

                let ci = runChunkIndex cameraPositionChunkIndex

                let loadAndGenerateChunk index = do

                        -- let ci = runChunkIndex index

                        createChunkMesh ref materials scene index options

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

            -- unload chunks
            do

                let ci = runChunkIndex cameraPositionChunkIndex
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

            -- update shadow rendering list
            if options.shadowEnabled
                then do

                    let cci = runChunkIndex cameraPositionChunkIndex
                    neighbors <- filterNeighbors options.shadowDisplayRange cci.x cci.y cci.z terrain.map
                    let meshes =  catMaybes ((\chunk -> case chunk.standardMaterialMesh of
                                MeshLoaded mesh -> Just (meshToAbstractMesh mesh)
                                _ -> Nothing
                            ) <$> neighbors)
                    setRenderList (meshes <> state.playerMeshes) shadowMap
                else do
                    setRenderList [] shadowMap

            -- picking
            do
                case state.mode of
                    Move -> pure unit
                    _ -> do
                        pickedBlock <- pickBlock scene cursor (State state) state.mousePosition.x state.mousePosition.y
                        case pickedBlock of
                            Nothing -> pure unit
                            Just bi -> void do
                                let rbi = runBlockIndex bi
                                r <- createVector3 (Int.toNumber rbi.x + 0.5) (Int.toNumber rbi.y + 0.5) (Int.toNumber rbi.z + 0.5)
                                setPosition r cursor
                                queryToHud driver (SetCursorPosition bi)

            do
                setIsVisible (case state.mode of
                    Put _ -> true
                    Remove -> true
                    Move -> false) (meshToAbstractMesh cursor)


foreign import foreachBlocks :: forall eff. Int -> Int -> Int -> Int -> Nullable ForeachIndex -> (Int -> Int -> Int -> Eff eff Int) -> Eff eff ForeachIndex

foreign import setTextContent :: forall eff. String -> String -> Eff (dom :: DOM | eff) Unit


