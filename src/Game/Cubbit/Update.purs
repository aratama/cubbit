module Game.Cubbit.Update (update, pickBlock, requestPointerLock, exitPointerLock) where

import Control.Alt (void)
import Control.Alternative (pure, when)
import Control.Bind (bind)
import Control.Coroutine (Consumer, consumer)
import Control.Monad.Aff (Aff, Canceler(..), launchAff, runAff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, error, errorShow, logShow)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF, Ref, modifyRef, newRef, readRef, writeRef)
import Control.Monad.Free (liftF)
import DOM (DOM)
import Data.Array (catMaybes, drop, take)
import Data.Foldable (for_)
import Data.Int (toNumber) as Int
import Data.Maybe (Maybe(..), isNothing)
import Data.Nullable (Nullable, toNullable)
import Data.Ord (abs, min)
import Data.Show (show)
import Data.Unit (Unit, unit)
import Data.Void (Void)
import Game.Cubbit.BlockIndex (BlockIndex, runBlockIndex)
import Game.Cubbit.Chunk (MeshLoadingState(MeshNotLoaded, MeshLoaded), disposeChunk)
import Game.Cubbit.ChunkIndex (chunkIndex, runChunkIndex)
import Game.Cubbit.ChunkMap (delete, filterNeighbors, getSortedChunks, size)
import Game.Cubbit.Hud (HudDriver, Query(..))
import Game.Cubbit.MeshBuilder (createChunkMesh)
import Game.Cubbit.Terrain (Terrain(Terrain), globalPositionToChunkIndex, globalPositionToGlobalIndex, isSolidBlock, lookupBlockByVec, lookupChunk, lookupSolidBlockByVec)
import Game.Cubbit.Types (Effects, CoreEffects, Mode(..), State(State), Materials, ForeachIndex, Options)
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
import Halogen (HalogenEffects, eventSource)
import Halogen.Query (action)
import Halogen.Query.EventSource (eventSource')
import Math (atan2, cos, max, pi, round, sin, sqrt)
import Network.HTTP.Affjax (AJAX)
import Prelude (negate, ($), (&&), (*), (+), (-), (/), (/=), (<), (<$>), (<>), (==), (||), type (~>))

playAnimation :: forall eff. String -> Ref State -> Eff (Effects eff) Unit
playAnimation name ref = do
    State state <- readRef ref
    for_ state.playerMeshes \mesh -> void do
        skeleton <- getSkeleton mesh
        animatable <- beginAnimation name true 1.0 pure skeleton
        when (isNothing animatable) do
            error ("playAnimation: animation named \"" <> name <> "\" not found.")

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


update :: forall eff. Ref State -> Scene -> Materials -> ShadowMap -> Mesh -> TargetCamera -> Options -> Mesh -> HudDriver (CoreEffects eff) -> Eff (Effects eff) Unit
update ref scene materials shadowMap cursor camera options skybox driver = do

        State state@{ terrain: Terrain terrain } <- readRef ref

        -- calculate next state
        do
            -- calculate player velocity
            let speed = options.moveSpeed

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
            let playerRotation' = if stopped || state.firstPersonView then state.playerRotation else (atan2 velocity.x velocity.z) - pi

            let playerPosition = {
                        x: state.position.x + velocity.x,
                        y: state.position.y + velocity.y,
                        z: state.position.z + velocity.z
                    }

            let animation' = if state.wKey || state.sKey || state.aKey || state.dKey then "run" else "idle"

            let globalIndex = runBlockIndex (globalPositionToGlobalIndex playerPosition.x playerPosition.y playerPosition.z)
            blockMaybe <- lookupBlockByVec playerPosition (Terrain terrain)

            let position' = case blockMaybe of
                                Just block | isSolidBlock block -> playerPosition {
                                    y = Int.toNumber (globalIndex.y) + 1.001
                                }
                                _ -> playerPosition

            let velocity' = case blockMaybe of
                                Just block | isSolidBlock block -> velocity { y = 0.0 }
                                _ -> velocity { y = velocity.y - 0.01 }

            -- camera view target
            let cameraSpeed = if state.firstPersonView then 0.5 else options.cameraTargetSpeed

            let eyeHeight = 1.4

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
            let thirdPersonCameraPositionY = position'.y + sin state.cameraPitch * state.cameraRange
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

                        totalFrames = state.totalFrames + 1
                    }

            -- update states
            writeRef ref (State state')

            when (animation' /= state.animation) do
                playAnimation animation' ref

            playerRotationVector <- createVector3 0.0 playerRotation' 0.0
            position <- createVector3 state'.position.x state'.position.y state'.position.z
            for_ state.playerMeshes \mesh -> void do
                AbstractMesh.setPosition position mesh
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

            skyboxRotationVector <- createVector3 0.0 (Int.toNumber state.totalFrames * 0.0001) 0.0
            setRotation skyboxRotationVector (meshToAbstractMesh skybox)


            -- load chunks
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
            do
                let cci = runChunkIndex cameraPositionChunkIndex
                neighbors <- filterNeighbors options.shadowDisplayRange cci.x cci.y cci.z terrain.map
                let meshes =  catMaybes ((\chunk -> case chunk.standardMaterialMesh of
                            MeshLoaded mesh -> Just (meshToAbstractMesh mesh)
                            _ -> Nothing
                        ) <$> neighbors)
                setRenderList (meshes <> state.playerMeshes) shadowMap

            -- picking
            do
                case state.mode of
                    Move -> pure unit
                    _ -> do
                        pickedBlock <- pickBlock scene cursor (State state) state.mousePosition.x state.mousePosition.y
                        case pickedBlock of
                            Nothing -> pure unit
                            Just bi -> do
                                let rbi = runBlockIndex bi
                                r <- createVector3 (Int.toNumber rbi.x + 0.5) (Int.toNumber rbi.y + 0.5) (Int.toNumber rbi.z + 0.5)
                                setPosition r cursor

                                setTextContent "cursor-position" (show rbi.x <> ", " <> show rbi.y <> ", " <> show rbi.z)

                                let e = eventSource

                                --let v = driver :: N eff2
                                --let t = driver.query :: Query ~> (Aff (HalogenEffects (ajax :: AJAX | eff2)))
                                --let u = driver.query (SetCursorPosition bi unit) :: (Aff (HalogenEffects (ajax :: AJAX | eff2))) Unit

                                --let p = runAff errorShow pure :: Aff (console :: CONSOLE | eff2) Unit -> Eff (console :: CONSOLE | eff2) (Canceler (console :: CONSOLE | eff2))



                                --let v = (driver.query (SetCursorPosition bi unit)) :: Aff (avar :: AVAR, ref :: REF, err :: EXCEPTION, dom :: DOM, ajax :: AJAX | eff)  Unit
                                --let s = launchAff (driver.query (SetCursorPosition bi unit)) -- :: forall eff. v Unit
                                -- let s = runAff logShow (\_ -> pure unit) (driver.query (SetCursorPosition bi unit))


                                pure unit


-- type HalogenIO f o m = { query :: f ~> m, subscribe :: CR.Consumer o m Unit -> m Unit }

-- = HalogenIO Query Void (Aff (HalogenEffects (ajax :: AJAX | eff)))
type N eff = {
    query :: Query ~> (Aff (HalogenEffects (ajax :: AJAX | eff))),
    subscribe :: Consumer Void (Aff (HalogenEffects (ajax :: AJAX | eff))) Unit -> (Aff (HalogenEffects (ajax :: AJAX | eff))) Unit
}

foreign import foreachBlocks :: forall eff. Int -> Int -> Int -> Int -> Nullable ForeachIndex -> (Int -> Int -> Int -> Eff eff Int) -> Eff eff ForeachIndex

foreign import setTextContent :: forall eff. String -> String -> Eff (dom :: DOM | eff) Unit

foreign import requestPointerLock :: ∀eff . ({ movementX :: Number, movementY :: Number } -> Eff (dom :: DOM | eff) Unit) -> Eff (dom :: DOM | eff) Unit -> Eff (dom :: DOM | eff) Unit

foreign import exitPointerLock :: ∀eff . Eff (dom :: DOM | eff) Unit
