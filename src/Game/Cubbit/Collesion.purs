module Game.Cubbit.Collesion (BodyTag, buildCollesionBoxes, updatePhysics, createPlayerCollesion, updateChunkCollesion, buildCollesionTerrain) where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Data.List (List(..), catMaybes, filter, (..), (:))
import Data.Map (delete, fromFoldable, insert, lookup, toList)
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.Traversable (for, for_)
import Data.Tuple (Tuple(..))
import Game.Cubbit.BlockType (BlockType)
import Game.Cubbit.Chunk (ChunkWithMesh)
import Game.Cubbit.ChunkIndex (ChunkIndex, chunkIndex, chunkIndexDistance, runChunkIndex)
import Game.Cubbit.Constants (chunkSize)
import Game.Cubbit.LocalIndex (LocalIndex, localIndex)
import Game.Cubbit.Terrain (Terrain(..), isSolidBlock, lookupChunk)
import Game.Cubbit.Types (State(..), SceneState(..))
import Graphics.Cannon (addShape, createBody, createMaterial, createSphere, createVec3, defaultBodyProps, setFixedRotation, setPosition, updateMassProperties)
import Graphics.Cannon.Body (getPosition, getVelocity, setVelocity)
import Graphics.Cannon.Type (CANNON, Body, World)
import Graphics.Cannon.Vec3 (runVec3)
import Graphics.Cannon.World (removeBody, step)
import Prelude (($), pure, bind, (/), (>>=), (<), (<=), (-), (+), (<$>))

type BodyTag = String

type BuildCollesionBoxesImports = {
    chunkSize :: Int,
    localIndex :: (Int -> Int -> Int -> LocalIndex),
    isSolidBlock :: BlockType -> Boolean
}



buildCollesionBoxes :: forall eff. ChunkWithMesh -> World -> Eff (cannon :: CANNON | eff) (Array Body)
buildCollesionBoxes = _buildCollesionBoxes { chunkSize, localIndex, isSolidBlock }

foreign import _buildCollesionBoxes :: forall eff. BuildCollesionBoxesImports -> ChunkWithMesh -> World -> Eff (cannon :: CANNON | eff) (Array Body)

createPlayerCollesion :: forall eff. Eff (cannon :: CANNON | eff) Body
createPlayerCollesion = do
    size <- createVec3 0.5 0.5 0.5
    pos <- createVec3 2.5 17.0 2.5
    mat <- createMaterial {
        friction: 0.0000,
        restitution: 0.0
    }

    body <- createBody defaultBodyProps {
        mass = 1.0,
        material = mat
    }

    upper <- createSphere 0.4
    upperOffset <- createVec3 0.0 (1.2) 0.0
    addShape upper (Just upperOffset) Nothing body
    lower <- createSphere 0.4
    lowerOffset <- createVec3 0.0 (0.4) 0.0
    addShape lower (Just lowerOffset) Nothing body

    setPosition pos body
    setFixedRotation true body
    updateMassProperties body
    pure body

updatePhysics :: forall eff. Number -> Body -> World -> State -> Eff (cannon :: CANNON | eff) State
updatePhysics deltaTime playerBox world (State state) = case state.sceneState of
    TitleSceneState t -> pure $ State state
    ModeSelectionSceneState t -> pure $ State state
    PlayingSceneState p -> do
        pos <- createVec3 p.position.x p.position.y p.position.z
        setPosition pos playerBox
        velocity <- createVec3 p.velocity.x p.velocity.y p.velocity.z
        setVelocity velocity playerBox

        -- step the world
        step (1.0 / 60.0) (1000.0 / deltaTime) 10 world

        -- read stepped world state
        posVec <- getPosition playerBox >>= runVec3
        velVec <- getVelocity playerBox >>= runVec3
        pure $ State state {
            sceneState = PlayingSceneState p {
                position = posVec,
                velocity = velVec
            }
        }





updateChunkCollesion :: forall eff. Terrain -> World -> ChunkIndex -> Eff (cannon :: CANNON | eff) Terrain
updateChunkCollesion (Terrain terrain) world index = do

    case lookup index terrain.bodies of
        Nothing -> pure (Terrain terrain)
        Just bodyLists -> do
            for_ bodyLists \body -> do
                removeBody body world

            let bodies' = delete index terrain.bodies

            chunkMaybe <- lookupChunk index (Terrain terrain)
            case chunkMaybe of
                Nothing -> pure $ Terrain terrain {
                    bodies = bodies'
                }
                Just chunk -> do
                    cannonBodies <- buildCollesionBoxes chunk world
                    pure $ Terrain terrain {
                        bodies = insert index cannonBodies $ delete index terrain.bodies
                    }

buildCollesionTerrain :: forall eff. Terrain -> World -> ChunkIndex -> Eff (cannon :: CANNON | eff) Terrain
buildCollesionTerrain (Terrain terrain) world index = do
    let ri = runChunkIndex index
    let xi = ri.x
    let yi = ri.y
    let zi = ri.z

    let bodyMap = toList terrain.bodies

    let externals = filter (\(Tuple k v) -> 1 < chunkIndexDistance k index) bodyMap
    for_ externals \(Tuple _ bodyLists) -> do
        for_ bodyLists \body -> do
            removeBody body world

    let internals :: List (Tuple ChunkIndex (Array Body))
        internals = filter (\(Tuple k v) -> chunkIndexDistance k index <= 1) bodyMap

    let internalsMap = fromFoldable internals


    let indices :: List ChunkIndex
        indices = do
            x <- (xi - 1) .. (xi + 1)
            y <- (yi - 1) .. (yi + 1)
            z <- (zi - 1) .. (zi + 1)
            let i = chunkIndex x y z
            case lookup i internalsMap of
                Just bodies -> mempty
                Nothing -> pure i


    chunks <- catMaybes <$> for indices (\i -> lookupChunk i (Terrain terrain))

    case chunks of
        Nil -> pure $ Terrain terrain
        Cons chunk _ -> do
            cannonBodies <- buildCollesionBoxes chunk world
            pure $ Terrain terrain {
                bodies = fromFoldable (Tuple chunk.index cannonBodies : internals)
            }
