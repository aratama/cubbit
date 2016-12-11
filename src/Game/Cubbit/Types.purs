module Game.Cubbit.Types where

import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Timer (TIMER)
import DOM (DOM)
import DOM.WebStorage (STORAGE)
import Data.Eq (class Eq)
import Data.Nullable (Nullable)
import Game.Cubbit.BlockType (BlockType(..))
import Game.Cubbit.Terrain (Terrain)
import Game.Cubbit.Vec (Vec)
import Graphics.Babylon.Types (AbstractMesh, BABYLON, Material, Sound)
import Graphics.Canvas (CANVAS)
import Network.HTTP.Affjax (AJAX)

type CoreEffects eff = (
    canvas :: CANVAS,
    now :: NOW,
    console :: CONSOLE,
    babylon :: BABYLON,
    ref :: REF,
    ajax :: AJAX,
    storage :: STORAGE,
    timer :: TIMER | eff)

type Effects eff =  CoreEffects (
    dom :: DOM,
    err :: EXCEPTION,
    avar :: AVAR | eff)

data Mode = Move | Put BlockType | Remove

newtype State = State {
    mode :: Mode,
    terrain :: Terrain,
    mousePosition :: { x :: Int, y :: Int },
    debugLayer :: Boolean,

    -- camera
    cameraPosition :: Vec,

    cameraTarget :: Vec,
    cameraYaw :: Number,
    cameraPitch :: Number,
    cameraRange :: Number,

    firstPersonViewPitch :: Number,
    firstPersonView :: Boolean,

    -- player physics
    position :: Vec,
    velocity :: Vec,
    playerRotation :: Number,
    playerPitch :: Number,

    totalFrames :: Int,
    minimap :: Boolean,

    playerMeshes :: Array AbstractMesh,

    updateIndex :: Nullable ForeachIndex,

    spaceKey :: Boolean,
    wKey :: Boolean,
    sKey :: Boolean,
    aKey :: Boolean,
    dKey :: Boolean,
    qKey :: Boolean,
    eKey :: Boolean,
    rKey :: Boolean,
    fKey :: Boolean,
    tKey :: Boolean,
    gKey :: Boolean,

    skyboxRotation :: Number,

    animation :: String,
    landing :: Int
}

type Materials = {
    blockMaterial :: Material,
    waterMaterial :: Material,
    cellShadingMaterial :: Material,
    bushMaterial :: Material,
    outlineMaterial :: Material
}





foreign import data ForeachIndex :: *


derive instance eqMode :: Eq Mode
