module Graphics.Babylon.Example.Sandbox.Types where

import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Eff.Ref (REF)
import DOM (DOM)
import Data.Maybe (Maybe)
import Graphics.Babylon (BABYLON)
import Graphics.Babylon.Example.Sandbox.Terrain (Terrain)
import Graphics.Babylon.Example.Sandbox.Vec (Vec)
import Graphics.Babylon.Material (Material)
import Graphics.Babylon.StandardMaterial (StandardMaterial)
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

    alicia :: Array AbstractMesh
}

type Materials = {
    boxMat :: Material,
    waterBoxMat :: Material
}
