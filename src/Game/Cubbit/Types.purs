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
import Graphics.Canvas (CANVAS)
import Network.HTTP.Affjax (AJAX)

type Effects eff = (canvas :: CANVAS, now :: NOW, console :: CONSOLE, dom :: DOM, babylon :: BABYLON, ref :: REF, ajax :: AJAX | eff)

data Mode = Move | Put | Remove

newtype State = State {
    mode :: Mode,
    terrain :: Terrain,
    mousePosition :: { x :: Int, y :: Int },
    debugLayer :: Boolean,

    -- player physics
    yaw :: Number,
    pitch :: Number,
    position :: Vec,
    velocity :: Vec,

    totalFrames :: Int,
    minimap :: Boolean,

    playerMeshes :: Array AbstractMesh,

    updateIndex :: Nullable ForeachIndex,

    wKey :: Boolean,
    sKey :: Boolean,
    aKey :: Boolean,
    dKey :: Boolean,
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
    cameraTargetSpeed :: Number
}

foreign import data ForeachIndex :: *