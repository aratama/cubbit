module Game.Cubbit.Types where

import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Eff.Ref (REF)
import DOM (DOM)
import Data.Nullable (Nullable)
import Game.Cubbit.Terrain (Terrain)
import Game.Cubbit.Vec (Vec)
import Graphics.Babylon (BABYLON)
import Graphics.Babylon.Material (Material)
import Graphics.Babylon.Types (AbstractMesh)
import Graphics.Babylon.Vector3 (Vector3)
import Graphics.Canvas (CANVAS)
import Network.HTTP.Affjax (AJAX)

type Effects eff = (canvas :: CANVAS, now :: NOW, console :: CONSOLE, dom :: DOM, babylon :: BABYLON, ref :: REF, ajax :: AJAX | eff)

data Mode = Move | Put | Remove

newtype State = State {
    mode :: Mode,
    terrain :: Terrain,
    mousePosition :: { x :: Int, y :: Int },
    debugLayer :: Boolean,

    -- camera
    cameraPosition :: Vec,

    viewReferencePoint :: Vec,
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
    animation :: String
}

type Materials = {
    blockMaterial :: Material,
    waterMaterial :: Material,
    cellShadingMaterial :: Material,
    bushMaterial :: Material
}

type Options = {
    loadDistance :: Int,
    fogDensity :: Number,
    maximumLoadedChunks :: Int,
    shadowDisplayRange :: Int,
    shadowMapSize :: Int,
    enableWaterMaterial :: Boolean,
    chunkUnloadSpeed :: Int,
    jumpVelocity :: Number,
    initialWorldSize :: Int,
    moveSpeed :: Number,
    cameraTargetSpeed :: Number,
    cameraRotationSpeed :: Number,
    cameraZoomSpeed :: Number,
    cameraMaxZ :: Number,
    cameraMinZ :: Number,
    pointerHorizontalSensitivity :: Number,
    pointerVerticalSensitivity :: Number
}

foreign import data ForeachIndex :: *
