module Game.Cubbit.Update (update, pickBlock) where

import Control.Alt (void)
import Control.Alternative (pure)
import Control.Bind (bind, when, (>>=))
import Control.Comonad.Store (store)
import Control.Comonad.Store.Class (seeks)
import Control.Monad.Eff (Eff, forE)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Ref (REF, Ref, modifyRef, newRef, readRef, writeRef)
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Control.MonadPlus (guard)
import DOM (DOM)
import Data.Array (head, (..))
import Data.Array.ST (emptySTArray, pushSTArray, runSTArray)
import Data.Foldable (for_)
import Data.Int (toNumber) as Int
import Data.List (List(..), elem, fromFoldable, notElem, nub, (:))
import Data.Maybe (Maybe(..), isNothing)
import Data.Nullable (Nullable, toNullable)
import Data.Ord (abs, min)
import Data.Show (show)
import Data.Unit (Unit, unit)
import Game.Cubbit.BlockIndex (BlockIndex, runBlockIndex)
import Game.Cubbit.Chunk (Chunk(..), ChunkWithMesh, MeshLoadingState(..))
import Game.Cubbit.ChunkIndex (chunkIndex, chunkIndexDistance, runChunkIndex)
import Game.Cubbit.ChunkMap (delete, peekAt)
import Game.Cubbit.Constants (loadDistance, unloadDistance)
import Game.Cubbit.MeshBuilder (createChunkMesh)
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
import Prelude (mod, ($), (+), (-), (/=), (<=), (<>), (==), (#), (<), (<=), negate, (&&))

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
                            chunkMaybe <- lookupChunk (chunkIndex dx dy dz) state.terrain
                            case chunkMaybe of
                                Just chunkData@{ standardMaterialMesh: MeshLoaded mesh } -> void do
                                    pushSTArray list (meshToAbstractMesh mesh)
                                _ -> pure unit
                pure list

            setRenderList chunks shadowMap



        -- load chunk
        do
            let costLimit = 100
            costRef <- newRef 0

            unit # tailRecM \_ -> do
                cost <- readRef costRef
                if costLimit < cost
                    then pure (Done unit)
                    else do
                        State st <- readRef ref
                        case st.updateList of
                            Nil -> pure (Done unit)
                            Cons index tail -> do
                                chunkMaybe <- lookupChunk index st.terrain
                                case chunkMaybe of
                                    Nothing -> void do
                                        modifyRef costRef ((+) 1)
                                    Just _ -> void do
                                        createChunkMesh ref materials scene index
                                        modifyRef costRef ((+) 10)
                                modifyRef ref (\(State st) -> State st {
                                    updateList = tail
                                })
                                pure (Loop unit)

            let loop s e f =    tailRecM (\i -> do
                                    cost <- readRef costRef
                                    if i <= e && cost < costLimit
                                        then do
                                            f i
                                            pure (Loop (i + 1))
                                        else pure (Done unit)
                                ) s



            let ci = runChunkIndex cameraPositionChunkIndex

            nextIndex <- foreachBlocks loadDistance ci.x ci.y ci.z state.updateIndex \x y z -> do
                let index = chunkIndex x y z
                State st <- readRef ref
                chunkMaybe <- lookupChunk index state.terrain
                case chunkMaybe of
                    Just _ -> do
                        --modifyRef costRef ((+) 0)
                        pure 1
                    Nothing -> do
                        result <- createChunkMesh ref materials scene index
                        -- log $ "load chunk: " <> show index
                        size <- chunkCount st.terrain
                        log $ "total chunks:" <> show (size + 1)

                        let i = runChunkIndex index
                        modifyRef ref (\(State st) -> State st {
                            updateList = nub (st.updateList <> fromFoldable [
                                chunkIndex (i.x - 1) i.y i.z,
                                chunkIndex (i.x + 1) i.y i.z,
                                chunkIndex i.x (i.y - 1) i.z,
                                chunkIndex i.x (i.y + 1) i.z,
                                chunkIndex i.x i.y (i.z - 1),
                                chunkIndex i.x i.y (i.z + 1)
                            ])
                        })

                        pure 100


            modifyRef ref \(State st) -> State st {
                updateIndex = toNullable (Just nextIndex)
            }


            forE (ci.x - 1) (ci.x + 1) \x ->
                forE (ci.y - 1) (ci.y + 1) \y ->
                    forE (ci.z - 1) (ci.z + 1) \z -> do
                        State st <- readRef ref
                        let index = chunkIndex x y z
                        when (notElem index st.pickableMeshList) do
                            chunkMaybe <- lookupChunk index st.terrain
                            case chunkMaybe of
                                Just { standardMaterialMesh: MeshLoaded mesh } -> void do
                                    setIsPickable true (meshToAbstractMesh mesh)
                                    AbstractMesh.setCheckCollisions true (meshToAbstractMesh mesh)
                                    writeRef ref $ State st {
                                        pickableMeshList = index : st.pickableMeshList
                                    }
                                _ -> pure unit


        -- unload sequence
        do
            State st@{ terrain: Terrain terrain@{ map } } <- readRef ref
            chunkMaybe <- peekAt st.unloadingChunkIndex map
            case chunkMaybe of
                Nothing -> pure unit
                Just chunkWithMesh@{ blocks: Chunk chunk } -> do

                    when (unloadDistance <= chunkIndexDistance cameraPositionChunkIndex chunk.index) do

                        when (chunk.index == chunkIndex 0 0 0) do
                            log "000"
                            log "000"
                            log "000"


                        disposeChunk chunkWithMesh
                        delete chunk.index map
                        let ci = runChunkIndex chunk.index
                        log ("unload: " <> show ci.x <> ", " <> show ci.y <> ", " <> show ci.z )
            writeRef ref $ State st {
                unloadingChunkIndex = st.unloadingChunkIndex + 1
            }




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





foreign import foreachBlocks :: forall eff. Int -> Int -> Int -> Int -> Nullable ForeachIndex -> (Int -> Int -> Int -> Eff eff Int) -> Eff eff ForeachIndex
