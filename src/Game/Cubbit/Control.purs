module Game.Cubbit.Control (playAnimation, pickBlock) where

import Control.Alt (void)
import Control.Alternative (pure, when)
import Control.Bind (bind)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (error)
import Control.Monad.Eff.Ref (REF, Ref, readRef)
import DOM (DOM)
import Data.Foldable (for_)
import Data.Int (toNumber) as Int
import Data.Maybe (Maybe(..), isNothing)
import Data.Ord (abs, min)
import Data.Unit (Unit, unit)
import Game.Cubbit.BlockIndex (BlockIndex, runBlockIndex)
import Game.Cubbit.Terrain (globalPositionToGlobalIndex, lookupSolidBlockByVec)
import Game.Cubbit.Types (Effects, Mode(Move, Remove, Put), State(State))
import Graphics.Babylon.AbstractMesh (abstractMeshToNode, getSkeleton)
import Graphics.Babylon.Mesh (setPosition)
import Graphics.Babylon.Node (getName)
import Graphics.Babylon.PickingInfo (getPickedPoint)
import Graphics.Babylon.Scene (pick)
import Graphics.Babylon.Skeleton (beginAnimation)
import Graphics.Babylon.Types (BABYLON, Mesh, Scene)
import Graphics.Babylon.Vector3 (createVector3, runVector3)
import Math (round)
import Prelude (($), (+), (-), (/=), (<>), (==))

playAnimation :: forall eff. String -> Ref State -> Eff (Effects eff) Unit
playAnimation name ref = do
    State state <- readRef ref
    for_ state.playerMeshes \mesh -> void do
        skeletonMaybe <- getSkeleton mesh
        case skeletonMaybe of
            Nothing -> pure unit
            Just skeleton -> do
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
                Put _ -> if minDelta == dx then do
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




