module Game.Cubbit.Update (update) where

import Control.Alt (void)
import Control.Alternative (pure, when)
import Control.Bind (bind)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff, runPure)
import Control.Monad.Eff.Ref (Ref, modifyRef, newRef, readRef, writeRef)
import DOM (DOM)
import Data.Array (catMaybes, drop, take)
import Data.Foldable (for_)
import Data.Int (toNumber) as Int
import Data.Maybe (Maybe(Just, Nothing))
import Data.Nullable (Nullable, toNullable)
import Data.Ord (min, max)
import Data.Tuple (Tuple(..))
import Data.Unit (Unit, unit)
import Data.Void (Void)
import Game.Cubbit.BlockIndex (runBlockIndex)
import Game.Cubbit.Chunk (MeshLoadingState(MeshNotLoaded, MeshLoaded), disposeChunk)
import Game.Cubbit.ChunkIndex (chunkIndex, runChunkIndex)
import Game.Cubbit.ChunkMap (delete, filterNeighbors, getSortedChunks, size)
import Game.Cubbit.Control (playAnimation, pickBlock)
import Game.Cubbit.Hud.Driver (queryToHud)
import Game.Cubbit.Hud.Type (Query(..), PlayingSceneQuery(..))
import Game.Cubbit.Materials (Materials)
import Game.Cubbit.MeshBuilder (createChunkMesh)
import Game.Cubbit.Option (Options(Options))
import Game.Cubbit.Sounds (Sounds)
import Game.Cubbit.Terrain (Terrain(Terrain), globalPositionToChunkIndex, globalPositionToGlobalIndex, isSolidBlock, lookupBlockByVec, lookupChunk)
import Game.Cubbit.Types (Effects, ForeachIndex, Mode(Move, Remove, Put), State(State), SceneState(..), PlayingSceneState)
import Game.Cubbit.Vec (vec, vecAdd, vecZero)
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
import Graphics.Babylon.Sound (play, stop)
import Graphics.Babylon.TargetCamera (setTarget, targetCameraToCamera)
import Graphics.Babylon.Types (Engine, Mesh, Scene, ShadowMap, TargetCamera)
import Graphics.Babylon.Vector3 (createVector3, length, subtract)
import Halogen (HalogenIO)
import Math (atan2, cos, pi, sin, sqrt)
import Prelude (negate, ($), (&&), (*), (+), (-), (/), (/=), (<), (<$>), (<>), (==), (||))

calcurateNextState :: Options -> Number -> State -> PlayingSceneState -> Tuple State PlayingSceneState
calcurateNextState (Options options) deltaTime (State state@{ terrain: Terrain terrain }) playingSceneState = runPure do

    let rot =  negate if playingSceneState.firstPersonView then (playingSceneState.playerRotation + pi) else  playingSceneState.cameraYaw

    let keyStep key = if key then 1.0 else 0.0

    let keyVector = {
            x: keyStep state.dKey - keyStep state.aKey,
            z: keyStep state.wKey - keyStep state.sKey
        }

    let rotatedKeyVector = {
            x: cos rot * keyVector.x - sin rot * keyVector.z,
            z: sin rot * keyVector.x + cos rot * keyVector.z
        }

    let stopped = rotatedKeyVector.x == 0.0 && rotatedKeyVector.z == 0.0

    let position = playingSceneState.position
    footHoldBlockMaybe <- lookupBlockByVec { x: position.x, y: position.y - 0.01, z: position.z } state.terrain

    let isLanding = case footHoldBlockMaybe of
            Just block | isSolidBlock block -> true
            _ -> false

    let jumpVelocity = if isLanding && state.spaceKey && playingSceneState.landing == 0 then options.jumpVelocity else 0.0

    let speed = options.moveSpeed * deltaTime

    let moveFactor = if isLanding then 1.0 else 0.2


    let gravityAccelerator = if isLanding then 0.0 else options.gravity * deltaTime

    let moveVectorLength = sqrt (rotatedKeyVector.x * rotatedKeyVector.x + rotatedKeyVector.z * rotatedKeyVector.z)
    let normalizedMoveX = if isLanding then rotatedKeyVector.x / moveVectorLength * speed else playingSceneState.velocity.x
    let normalizedMoveZ = if isLanding then rotatedKeyVector.z / moveVectorLength * speed else playingSceneState.velocity.z


    let velocityX = if isLanding then (if stopped then playingSceneState.velocity.x * 0.5 else normalizedMoveX) else playingSceneState.velocity.x
    let velocityY = playingSceneState.velocity.y + jumpVelocity + gravityAccelerator
    let velocityZ = if isLanding then (if stopped then playingSceneState.velocity.z * 0.5 else normalizedMoveZ) else playingSceneState.velocity.z
    let velocity = if 0 < playingSceneState.landing then vecZero else vec velocityX velocityY velocityZ

    -- playerRotation == 0   =>    -z direction
    let playerRotation' = if isLanding
            then (if 0 < playingSceneState.landing
                then playingSceneState.playerRotation
                else if stopped || playingSceneState.firstPersonView
                    then playingSceneState.playerRotation
                    else (atan2 velocity.x velocity.z) - pi
            ) else playingSceneState.playerRotation

    let playerPosition = vecAdd playingSceneState.position velocity



    let animation' = if 0 < playingSceneState.landing
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




    footHoldBlockMaybe' <- lookupBlockByVec { x: position'.x, y: position'.y - 0.01, z: position'.z } state.terrain
    let isLanding' = case footHoldBlockMaybe' of
            Just block | isSolidBlock block -> true
            _ -> false

    let landingCount = if isLanding' && playingSceneState.velocity.y < options.landingVelocityLimit then options.landingDuration else playingSceneState.landing

    -- camera view target
    let cameraSpeed = if playingSceneState.firstPersonView then 0.5 else options.cameraTargetSpeed

    let eyeHeight = options.eyeHeight

    let thirdPersonCameraTargetX = position'.x
    let thirdPersonCameraTargetY = position'.y + eyeHeight
    let thirdPersonCameraTargetZ = position'.z

    let playerRotationTheta = negate playingSceneState.playerRotation - pi * 0.5
    let firstPersonCameraTargetX = position'.x + cos playerRotationTheta * cos playingSceneState.playerPitch
    let firstPersonCameraTargetY = position'.y + eyeHeight + sin playingSceneState.playerPitch
    let firstPersonCameraTargetZ = position'.z + sin playerRotationTheta * cos playingSceneState.playerPitch

    let cameraTargetX' = if playingSceneState.firstPersonView then firstPersonCameraTargetX else thirdPersonCameraTargetX
    let cameraTargetY' = if playingSceneState.firstPersonView then firstPersonCameraTargetY else thirdPersonCameraTargetY
    let cameraTargetZ' = if playingSceneState.firstPersonView then firstPersonCameraTargetZ else thirdPersonCameraTargetZ

    let cameraTargetInterpolatedX' = state.cameraTarget.x + (cameraTargetX' - state.cameraTarget.x) * cameraSpeed
    let cameraTargetInterpolatedY' = state.cameraTarget.y + (cameraTargetY' - state.cameraTarget.y) * cameraSpeed
    let cameraTargetInterpolatedZ' = state.cameraTarget.z + (cameraTargetZ' - state.cameraTarget.z) * cameraSpeed

    let cameraTarget' = { x: cameraTargetInterpolatedX', y: cameraTargetInterpolatedY', z: cameraTargetInterpolatedZ' }

    -- camera position
    let cameraPosition = state.cameraPosition
    let cameraPositionChunkIndex = globalPositionToChunkIndex cameraPosition.x cameraPosition.y cameraPosition.z

    let theta = negate playingSceneState.cameraYaw - pi * 0.5
    let thirdPersonCameraPositionX = position'.x + cos theta * cos playingSceneState.cameraPitch * playingSceneState.cameraRange
    let thirdPersonCameraPositionY = position'.y + eyeHeight + sin playingSceneState.cameraPitch * playingSceneState.cameraRange
    let thirdPersonCameraPositionZ = position'.z + sin theta * cos playingSceneState.cameraPitch * playingSceneState.cameraRange

    let firstPersonCameraPositionX = position'.x
    let firstPersonCameraPositionY = position'.y + eyeHeight
    let firstPersonCameraPositionZ = position'.z

    let cameraPositionX = if playingSceneState.firstPersonView then firstPersonCameraPositionX else thirdPersonCameraPositionX
    let cameraPositionY = if playingSceneState.firstPersonView then firstPersonCameraPositionY else thirdPersonCameraPositionY
    let cameraPositionZ = if playingSceneState.firstPersonView then firstPersonCameraPositionZ else thirdPersonCameraPositionZ

    let cameraPositionInterpolatedX = cameraPosition.x + (cameraPositionX - cameraPosition.x) * cameraSpeed
    let cameraPositionInterpolatedY = cameraPosition.y + (cameraPositionY - cameraPosition.y) * cameraSpeed
    let cameraPositionInterpolatedZ = cameraPosition.z + (cameraPositionZ - cameraPosition.z) * cameraSpeed

    -- final state


    let sceneState =  playingSceneState {
                cameraYaw = playingSceneState.cameraYaw + ((if state.qKey then 1.0 else 0.0) - (if state.eKey then 1.0 else 0.0)) * options.cameraRotationSpeed,
                cameraPitch = max 0.1 (min (pi * 0.48) (playingSceneState.cameraPitch + ((if state.rKey then 1.0 else 0.0) - (if state.fKey then 1.0 else 0.0)) * options.cameraRotationSpeed)),
                cameraRange = max options.cameraMinimumRange (min options.cameraMaximumRange (playingSceneState.cameraRange + ((if state.gKey then 1.0 else 0.0) - (if state.tKey then 1.0 else 0.0)) * options.cameraZoomSpeed)),
                position = position',
                velocity = velocity,
                playerRotation = playerRotation',
                animation = animation',
                landing = max 0 (landingCount - 1)
            }


    let state' = state {

                sceneState = PlayingSceneState sceneState,

                cameraTarget = cameraTarget',
                cameraPosition = { x:cameraPositionInterpolatedX, y: cameraPositionInterpolatedY, z: cameraPositionInterpolatedZ },
                totalFrames = state.totalFrames + 1,
                skyboxRotation = state.skyboxRotation + options.skyboxRotationSpeed * deltaTime
            }

    pure (Tuple (State state') sceneState)

update :: forall eff. Ref State
                   -> Engine
                   -> Scene
                   -> Materials
                   -> Sounds
                   -> ShadowMap
                   -> Mesh
                   -> TargetCamera
                   -> Options
                   -> Mesh
                   -> HalogenIO Query Void (Aff (Effects eff))
                   -> Eff (Effects eff) Unit
update ref engine scene materials sounds shadowMap cursor camera (Options options) skybox driver = do

        State state@{ terrain: Terrain terrain } <- readRef ref

        deltaTime <- getDeltaTime engine

        case state.sceneState of

            TitleSceneState -> pure unit

            PlayingSceneState playingSceneState -> do


                Tuple (State state') playingSceneState' <- pure $ calcurateNextState (Options options) deltaTime (State state) playingSceneState

                writeRef ref (State state')


                let cameraPosition = state.cameraPosition
                let cameraPositionChunkIndex = globalPositionToChunkIndex state'.cameraPosition.x state'.cameraPosition.y state'.cameraPosition.z


                when (playingSceneState'.animation /= playingSceneState.animation) do
                    playAnimation playingSceneState'.animation playingSceneState

                playerRotationVector <- createVector3 0.0 playingSceneState'.playerRotation 0.0
                positionVector <- createVector3 playingSceneState'.position.x playingSceneState'.position.y playingSceneState'.position.z
                for_ playingSceneState.playerMeshes \mesh -> void do
                    AbstractMesh.setPosition positionVector mesh
                    setRotation playerRotationVector mesh
                    setVisibility (if playingSceneState.firstPersonView then 0.0 else 1.0) mesh

                -- update camera

                cameraPositionVector <- createVector3 state'.cameraPosition.x state'.cameraPosition.y state'.cameraPosition.z

                cameraTargetVector <- createVector3 state'.cameraTarget.x state'.cameraTarget.y state'.cameraTarget.z

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

                            createChunkMesh ref materials scene index (Options options)

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
                        setRenderList (meshes <> playingSceneState.playerMeshes) shadowMap
                    else do
                        setRenderList [] shadowMap

                -- picking
                do
                    case playingSceneState.mode of
                        Move -> pure unit
                        _ -> do
                            pickedBlock <- pickBlock scene cursor playingSceneState.mode state.terrain state.mousePosition.x state.mousePosition.y
                            case pickedBlock of
                                Nothing -> pure unit
                                Just bi -> void do
                                    let rbi = runBlockIndex bi
                                    r <- createVector3 (Int.toNumber rbi.x + 0.5) (Int.toNumber rbi.y + 0.5) (Int.toNumber rbi.z + 0.5)
                                    setPosition r cursor
                                    queryToHud driver (PlayingSceneQuery (SetCursorPosition bi))

                do
                    setIsVisible (case playingSceneState.mode of
                        Put _ -> true
                        Remove -> true
                        Move -> false) (meshToAbstractMesh cursor)

                -- sounds
                do
                    if playingSceneState.animation /= "run" && playingSceneState'.animation == "run"
                        then play sounds.stepSound
                        else if playingSceneState.animation == "run" && playingSceneState'.animation /= "run"
                            then stop sounds.stepSound
                            else pure unit


foreign import foreachBlocks :: forall eff. Int -> Int -> Int -> Int -> Nullable ForeachIndex -> (Int -> Int -> Int -> Eff eff Int) -> Eff eff ForeachIndex

foreign import setTextContent :: forall eff. String -> String -> Eff (dom :: DOM | eff) Unit


