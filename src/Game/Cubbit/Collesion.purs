module Game.Cubbit.Collesion (BodyTag, buildCollesionBoxes, updatePhysics, createPlayerCollesion) where

import Control.Monad.Eff (Eff)
import Data.Maybe (Maybe(..))
import Game.Cubbit.Chunk (ChunkWithMesh)
import Game.Cubbit.Constants (chunkSize)
import Game.Cubbit.LocalIndex (LocalIndex, localIndex)
import Game.Cubbit.Types (State(..), SceneState(..))
import Graphics.Cannon (addBody, addShape, createBody, createMaterial, createSphere, createVec3, createWorld, defaultBodyProps, setFixedRotation, setGravity, setPosition, setTag, updateMassProperties)
import Graphics.Cannon.Body (getPosition, getVelocity, setVelocity)
import Graphics.Cannon.Type (CANNON, Body, CANNON, World)
import Graphics.Cannon.Vec3 (runVec3)
import Graphics.Cannon.World (step)
import Prelude (($), pure, bind, (/), (>>=))

type BodyTag = String

type BuildCollesionBoxesImports = {
    chunkSize :: Int,
    localIndex :: (Int -> Int -> Int -> LocalIndex)
}

buildCollesionBoxes :: forall eff. ChunkWithMesh -> World BodyTag -> Eff (cannon :: CANNON | eff) (Array (Body BodyTag))
buildCollesionBoxes = _buildCollesionBoxes { chunkSize, localIndex }

foreign import _buildCollesionBoxes :: forall eff. BuildCollesionBoxesImports -> ChunkWithMesh -> World BodyTag -> Eff (cannon :: CANNON | eff) (Array (Body BodyTag))

createPlayerCollesion :: forall eff. Eff (cannon :: CANNON | eff) (Body String)
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
    setTag (Just "player") body
    setFixedRotation true body
    updateMassProperties body
    pure body

updatePhysics :: forall eff. Number -> Body String -> World String -> State -> Eff (cannon :: CANNON | eff) State
updatePhysics deltaTime playerBox world (State state) = case state.sceneState of
    TitleSceneState t -> pure $ State state
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
