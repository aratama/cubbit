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

type Effects eff = (canvas :: CANVAS, now :: NOW, console :: CONSOLE, dom :: DOM, babylon :: BABYLON, ref :: REF | eff)

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

    updateIndex :: Nullable ForeachIndex
}

type Materials = {
    boxMat :: Material,
    waterBoxMat :: Material,
    cellShadingMaterial :: Material,
    bushMaterial :: Material
}

foreign import data ForeachIndex :: *