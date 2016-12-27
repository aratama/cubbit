module Game.Cubbit.Update (update, updateBabylon, updateSound) where

import Control.Alt (void)
import Control.Alternative (pure, when)
import Control.Bind (bind)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff, runPure)
import Control.Monad.Eff.Console (error)
import Control.Monad.Eff.Timer (TIMER, setTimeout)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.HTMLElement (getBoundingClientRect)
import DOM.HTML.Types (htmlDocumentToNonElementParentNode)
import DOM.HTML.Window (document)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (ElementId(ElementId))
import Data.Array (catMaybes, drop, take, any)
import Data.BooleanAlgebra (not)
import Data.Foldable (for_)
import Data.Int (floor)
import Data.Int (toNumber) as Int
import Data.Maybe (Maybe(Just, Nothing))
import Data.Nullable (Nullable, toMaybe, toNullable)
import Data.Ord (min, max)
import Data.Set (member)
import Data.Tuple (Tuple(..))
import Data.Unit (Unit, unit)
import Data.Void (Void)
import Game.Cubbit.BlockIndex (runBlockIndex, blockIndexDistance)
import Game.Cubbit.ChunkInstance (MeshLoadingState(MeshNotLoaded, MeshLoaded), disposeChunk)
import Game.Cubbit.ChunkIndex (chunkIndex, runChunkIndex)
import Game.Cubbit.ChunkMap (delete, filterNeighbors, getSortedChunks, size)
import Game.Cubbit.Config (Config(..))
import Game.Cubbit.Constants (sliderMaxValue)
import Game.Cubbit.Control (playAnimation, pickBlock)
import Game.Cubbit.Hud.Driver (queryToHud)
import Game.Cubbit.Hud.Type (Query(..), QueryA(..), PlayingSceneQuery(..))
import Game.Cubbit.MeshBuilder (generateChunk)
import Game.Cubbit.Option (Options(Options))
import Game.Cubbit.Resources (Resources)
import Game.Cubbit.Sounds (Sounds)
import Game.Cubbit.Terrain (Terrain(Terrain), globalPositionToChunkIndex, globalPositionToGlobalIndex, isSolidBlock, lookupBlockByVec, lookupChunk)
import Game.Cubbit.Types (Effects, ForeachIndex, Mode(Move, Remove, Put), PlayingSceneState, SceneState(PlayingSceneState, ModeSelectionSceneState, TitleSceneState), State(State))
import Graphics.Babylon.AbstractMesh (abstractMeshToNode, setIsVisible, setRotation, setVisibility)
import Graphics.Babylon.AbstractMesh (setPosition) as AbstractMesh
import Graphics.Babylon.Camera (setPosition) as Camera
import Graphics.Babylon.Mesh (meshToAbstractMesh, setPosition)
import Graphics.Babylon.Node (getName)
import Graphics.Babylon.PickingInfo (getPickedPoint)
import Graphics.Babylon.Ray (createRayWithLength)
import Graphics.Babylon.Scene (pickWithRay)
import Graphics.Babylon.ShadowGenerator (setRenderList)
import Graphics.Babylon.Sound (play, setVolume, stop)
import Graphics.Babylon.TargetCamera (setTarget, targetCameraToCamera)
import Graphics.Babylon.Types (BABYLON)
import Graphics.Babylon.Vector3 (createVector3, length, subtract)
import Halogen (HalogenIO)
import Math (atan2, cos, pi, sin, sqrt)
import Prelude (negate, ($), (&&), (*), (+), (-), (/), (/=), (<), (<$>), (<>), (==), (||), (>>=))
import Unsafe.Coerce (unsafeCoerce)


epsiron :: Number
epsiron = 0.1

calcurateNextState :: Options -> Number -> State -> PlayingSceneState -> Tuple State PlayingSceneState
calcurateNextState (Options options) deltaTime (State state@{ terrain: Terrain terrain }) playingSceneState = runPure do

    let rot =  negate if playingSceneState.firstPersonView then (playingSceneState.playerRotation + pi) else  playingSceneState.cameraYaw

    let keyStep key = if member key state.keys then 1.0 else 0.0

    let keyVector = {
            x: keyStep "d" - keyStep "a",
            z: keyStep "w" - keyStep "s"
        }

    let rotatedKeyVector = {
            x: cos rot * keyVector.x - sin rot * keyVector.z,
            z: sin rot * keyVector.x + cos rot * keyVector.z
        }

    let stopped = rotatedKeyVector.x == 0.0 && rotatedKeyVector.z == 0.0

    let position = playingSceneState.position
    footHoldBlockMaybe <- lookupBlockByVec { x: position.x, y: position.y - epsiron, z: position.z } state.terrain

    let isLanding = case footHoldBlockMaybe of
            Just block | isSolidBlock block -> true
            _ -> false

    let isStartingJump = playingSceneState.jumpable && isLanding && member " " state.keys && playingSceneState.landing == 0
    let jumpVelocity = if isStartingJump then options.jumpVelocity else 0.0

    let speed = options.moveSpeed

    let moveFactor = if isLanding then 1.0 else 0.2


    let gravityAccelerator = if isLanding then 0.0 else options.gravity * deltaTime

    let moveVectorLength = sqrt (rotatedKeyVector.x * rotatedKeyVector.x + rotatedKeyVector.z * rotatedKeyVector.z)

    let velocity = if 0.0 < moveVectorLength && playingSceneState.landing == 0
            then {
                x: rotatedKeyVector.x / moveVectorLength * speed,
                y: playingSceneState.velocity.y + jumpVelocity,
                z: rotatedKeyVector.z / moveVectorLength * speed
            }
            else {
                x: 0.0,
                y: playingSceneState.velocity.y + jumpVelocity,
                z: 0.0
            }

    -- playerRotation == 0   =>    -z direction
    let playerRotation' = if isLanding
            then (if 0 < playingSceneState.landing
                then playingSceneState.playerRotation
                else if stopped || playingSceneState.firstPersonView
                    then playingSceneState.playerRotation
                    else (atan2 velocity.x velocity.z) - pi
            ) else playingSceneState.playerRotation

    let playerPosition = {
                x: playingSceneState.position.x + velocity.x * deltaTime,
                y: playingSceneState.position.y + velocity.y * deltaTime,
                z: playingSceneState.position.z + velocity.z * deltaTime
            }



    let animation' = if 0 < playingSceneState.landing
            then "land"
            else if isLanding
                then (if any (\k -> member k state.keys) ["w", "s", "a", "d"] then "run" else "idle")
                else "jump"

    let globalIndex = runBlockIndex (globalPositionToGlobalIndex playerPosition.x playerPosition.y playerPosition.z)
    blockMaybe <- lookupBlockByVec playerPosition (Terrain terrain)



    footHoldBlockMaybe' <- lookupBlockByVec {
        x: playingSceneState.position.x,
        y: playingSceneState.position.y - 0.01,
        z: playingSceneState.position.z
    } state.terrain
    let isLanding' = case footHoldBlockMaybe' of
            Just block | isSolidBlock block -> true
            _ -> false

    let landingCount = if isLanding' && playingSceneState.velocity.y < options.landingVelocityLimit
            then options.landingDuration
            else playingSceneState.landing

    -- camera view target
    let cameraSpeed = if playingSceneState.firstPersonView then 0.5 else options.cameraTargetSpeed

    let eyeHeight = options.eyeHeight

    let thirdPersonCameraTargetoffset = 2.0

    let thirdPersonCameraTargetX = playingSceneState.position.x             + velocity.x * thirdPersonCameraTargetoffset
    let thirdPersonCameraTargetY = playingSceneState.position.y + eyeHeight
    let thirdPersonCameraTargetZ = playingSceneState.position.z             + velocity.z * thirdPersonCameraTargetoffset

    let playerRotationTheta = negate playingSceneState.playerRotation - pi * 0.5
    let firstPersonCameraTargetX = playingSceneState.position.x + cos playerRotationTheta * cos playingSceneState.playerPitch
    let firstPersonCameraTargetY = playingSceneState.position.y + eyeHeight + sin playingSceneState.playerPitch
    let firstPersonCameraTargetZ = playingSceneState.position.z + sin playerRotationTheta * cos playingSceneState.playerPitch

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

    let thirdPersonCameraPositionX = playingSceneState.position.x + cos theta * cos playingSceneState.cameraPitch * playingSceneState.cameraRange + velocity.x * thirdPersonCameraTargetoffset
    let thirdPersonCameraPositionY = playingSceneState.position.y + eyeHeight + sin playingSceneState.cameraPitch * playingSceneState.cameraRange
    let thirdPersonCameraPositionZ = playingSceneState.position.z + sin theta * cos playingSceneState.cameraPitch * playingSceneState.cameraRange + velocity.z * thirdPersonCameraTargetoffset

    let firstPersonCameraPositionX = playingSceneState.position.x
    let firstPersonCameraPositionY = playingSceneState.position.y + eyeHeight
    let firstPersonCameraPositionZ = playingSceneState.position.z

    let cameraPositionX = if playingSceneState.firstPersonView then firstPersonCameraPositionX else thirdPersonCameraPositionX
    let cameraPositionY = if playingSceneState.firstPersonView then firstPersonCameraPositionY else thirdPersonCameraPositionY
    let cameraPositionZ = if playingSceneState.firstPersonView then firstPersonCameraPositionZ else thirdPersonCameraPositionZ

    let cameraPositionInterpolatedX = cameraPosition.x + (cameraPositionX - cameraPosition.x) * cameraSpeed
    let cameraPositionInterpolatedY = cameraPosition.y + (cameraPositionY - cameraPosition.y) * cameraSpeed
    let cameraPositionInterpolatedZ = cameraPosition.z + (cameraPositionZ - cameraPosition.z) * cameraSpeed

    -- final state


    let sceneState =  playingSceneState {
                cameraYaw = playingSceneState.cameraYaw + ((if member "q" state.keys then 1.0 else 0.0) - (if member "e" state.keys then 1.0 else 0.0)) * options.cameraRotationSpeed,
                cameraPitch = max 0.1 (min (pi * 0.48) (playingSceneState.cameraPitch + ((if member "r" state.keys then 1.0 else 0.0) - (if member "f" state.keys then 1.0 else 0.0)) * options.cameraRotationSpeed)),
                cameraRange = max options.cameraMinimumRange (min options.cameraMaximumRange (playingSceneState.cameraRange + ((if member "g" state.keys then 1.0 else 0.0) - (if member "t" state.keys then 1.0 else 0.0)) * options.cameraZoomSpeed)),
                velocity = velocity,
                playerRotation = playerRotation',
                animation = animation',
                landing = max 0 (landingCount - 1),
                jumpable = if isLanding && not (member " " state.keys) then true else if member " " state.keys then false else playingSceneState.jumpable
            }


    let state' = state {

                sceneState = PlayingSceneState sceneState,

                cameraTarget = cameraTarget',
                cameraPosition = { x:cameraPositionInterpolatedX, y: cameraPositionInterpolatedY, z: cameraPositionInterpolatedZ },
                totalFrames = state.totalFrames + 1,
                skyboxRotation = state.skyboxRotation + options.skyboxRotationSpeed * deltaTime
            }

    pure (Tuple (State state') sceneState)





update :: forall eff. Number
                    -> Resources
                   -> HalogenIO Query Void (Aff (Effects eff))
                   -> State
                   -> Eff (Effects eff) State
update deltaTime res@{ options: Options options } driver (State state@{ terrain: Terrain terrain }) = do
        case state.sceneState of

            TitleSceneState titleSceneState -> do
                let state' = state {
                        cameraPosition = { x: 5.0 - titleSceneState.position, y: 20.0, z: negate 10.0 + titleSceneState.position },
                        cameraTarget = { x: 0.5 - titleSceneState.position, y: 11.0, z: titleSceneState.position },
                        sceneState = TitleSceneState titleSceneState {
                            position = titleSceneState.position + deltaTime * 0.0008
                        }
                    }
                pure (State state')

            ModeSelectionSceneState ms -> pure (State state)

            PlayingSceneState playingSceneState -> do

                Tuple (State state') playingSceneState' <- pure $ calcurateNextState (Options options) deltaTime (State state) playingSceneState

                when (playingSceneState'.animation /= playingSceneState.animation) do
                    playAnimation playingSceneState'.animation res.playerMeshes

                playerRotationVector <- createVector3 0.0 playingSceneState'.playerRotation 0.0
                positionVector <- createVector3 playingSceneState'.position.x playingSceneState'.position.y playingSceneState'.position.z
                for_ res.playerMeshes \mesh -> void do
                    AbstractMesh.setPosition positionVector mesh
                    setRotation playerRotationVector mesh
                    setVisibility (if playingSceneState.firstPersonView then 0.0 else 1.0) mesh


                -- picking
                do
                    case playingSceneState'.mode of
                        Move -> pure unit
                        _ -> do

                            canvasMaybe <- toMaybe <$> ((htmlDocumentToNonElementParentNode <$> (window >>= document)) >>= getElementById (ElementId "renderCanvas"))
                            screenCenter <- case canvasMaybe of
                                Nothing -> do
                                    error "Canvas not found"
                                    pure { x: 0, y: 0 }
                                Just canvas -> do
                                    bounds <- getBoundingClientRect (unsafeCoerce canvas)
                                    pure {
                                        x: floor (bounds.width * 0.5),
                                        y: floor (bounds.height * 0.5)
                                    }

                            let playerPositionIndex = globalPositionToGlobalIndex playingSceneState'.position.x playingSceneState'.position.y playingSceneState'.position.z

                            pickedBlock <- if playingSceneState'.firstPersonView
                                then pickBlock res.scene res.cursor playingSceneState'.mode state.terrain screenCenter.x screenCenter.y
                                else pickBlock res.scene res.cursor playingSceneState'.mode state.terrain state.mousePosition.x state.mousePosition.y
                            case pickedBlock of
                                Nothing -> pure unit
                                Just bi -> void do


                                    when (blockIndexDistance playerPositionIndex bi < options.blockPickingMaxDistance) do

                                        let rbi = runBlockIndex bi
                                        r <- createVector3 (Int.toNumber rbi.x + 0.5) (Int.toNumber rbi.y + 0.5) (Int.toNumber rbi.z + 0.5)
                                        setPosition r res.cursor
                                        queryToHud driver (Query (PlayingSceneQuery (SetCursorPosition bi)))


                do
                    setIsVisible (case playingSceneState'.mode of
                        Put _ -> true
                        Remove -> true
                        Move -> false) (meshToAbstractMesh res.cursor)

                -- sounds
                do
                    if playingSceneState.animation /= "run" && playingSceneState'.animation == "run"
                        then play res.sounds.stepSound
                        else if playingSceneState.animation == "run" && playingSceneState'.animation /= "run"
                            then stop res.sounds.stepSound
                            else pure unit

                pure (State state')




updateBabylon :: forall eff. Number
                   -> Resources
                   -> State
                   -> Eff (babylon :: BABYLON, timer :: TIMER | eff)  State
updateBabylon deltaTime res@{ options: Options options } (State state@{ terrain: Terrain terrain }) = do

        Config config <- pure state.config

        -- update camera
        let cameraPosition = state.cameraPosition
        let cameraPositionChunkIndex = globalPositionToChunkIndex state.cameraPosition.x state.cameraPosition.y state.cameraPosition.z

        cameraPositionVector <- createVector3 state.cameraPosition.x state.cameraPosition.y state.cameraPosition.z
        cameraTargetVector <- createVector3 state.cameraTarget.x state.cameraTarget.y state.cameraTarget.z

        cameraPositionBlockMaybe <- lookupBlockByVec cameraPosition state.terrain
        cameraPosition''  <- case cameraPositionBlockMaybe of

            Just block | isSolidBlock block -> do

                cameraDirection <- subtract cameraTargetVector cameraPositionVector
                cameraDirectionLength <- length cameraDirection
                cameraRay <- createRayWithLength cameraPositionVector cameraDirection cameraDirectionLength
                let predicate mesh = do
                        let name = getName (abstractMeshToNode mesh)
                        pure (name == "terrain")
                picked <- pickWithRay cameraRay predicate true res.scene

                let pickedPoint = getPickedPoint picked

                pure case getPickedPoint picked of
                        Nothing -> cameraPositionVector
                        Just point -> point

            _ -> pure cameraPositionVector

        Camera.setPosition cameraPosition'' (targetCameraToCamera res.targetCamera)
        setTarget cameraTargetVector res.targetCamera

        skyboxRotationVector <- createVector3 0.0 state.skyboxRotation 0.0
        setRotation skyboxRotationVector (meshToAbstractMesh res.skybox)


        -- load chunks
        nextState <- do

            let ci = runChunkIndex cameraPositionChunkIndex

            let loadAndGenerateChunk index = generateChunk (State state) res.materials res.scene index (Options options) state.config

            let loadDistance = 3 + config.chunkArea
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




            pure $ State state {
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
        if config.shadow
            then do
                let shadowDisplayRange = 1 + config.shadowArea
                let cci = runChunkIndex cameraPositionChunkIndex
                neighbors <- filterNeighbors shadowDisplayRange cci.x cci.y cci.z terrain.map
                let meshes =  catMaybes ((\chunk -> case chunk.standardMaterialMesh of
                            MeshLoaded mesh -> Just (meshToAbstractMesh mesh)
                            _ -> Nothing
                        ) <$> neighbors)
                setRenderList (meshes <> res.playerMeshes) res.shadowMap
            else do
                setRenderList [] res.shadowMap


        pure nextState



updateSound :: forall eff. Sounds -> State -> Eff (babylon :: BABYLON, timer :: TIMER | eff)  State
updateSound sounds (State state@{ config: Config config }) = do

    -- update volumes
    let sliderValueToNumber n = if config.mute then 0.0 else Int.toNumber n / Int.toNumber sliderMaxValue
    for_ sounds.bgms $ setVolume $ sliderValueToNumber config.bgmVolume * state.volume
    for_ sounds.ses $ setVolume $ sliderValueToNumber config.seVolume

    case state.bgm, state.nextBGM, state.volume of
        Just bgm, Just next, 0.0 -> void do
            stop bgm
            setTimeout 1000 $ play next
        Nothing, Just next, 0.0 -> play next
        _, _, _ -> pure unit

    -- calc next state
    pure $ State state {

        bgm = case state.nextBGM, state.volume of
            Just next, 0.0 -> Just next
            _, _ -> state.bgm,

        nextBGM = case state.nextBGM, state.volume of
            Just next, 0.0 -> Nothing
            _, _ -> state.nextBGM,

        volume = case state.nextBGM of
            Just _ -> max 0.0 $ state.volume - 0.02
            _ -> 1.0
    }



foreign import foreachBlocks :: forall eff. Int -> Int -> Int -> Int -> Nullable ForeachIndex -> (Int -> Int -> Int -> Eff eff Int) -> Eff eff ForeachIndex

foreign import setTextContent :: forall eff. String -> String -> Eff (dom :: DOM | eff) Unit


