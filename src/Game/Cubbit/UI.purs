module Game.Cubbit.UI (initializeUI) where

import Control.Alternative (pure)
import Control.Bind (bind)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (Ref, modifyRef, readRef)
import Data.Int (toNumber)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Unit (Unit, unit)
import Game.Cubbit.BlockType (dirtBlock, airBlock)
import Game.Cubbit.Event (onMouseClick, onRightMouseDrag, onWheel)
import Game.Cubbit.MeshBuilder (editBlock)
import Game.Cubbit.Types (Effects, Materials, Mode(..), State(State), Options)
import Game.Cubbit.Control (pickBlock)
import Graphics.Babylon (Canvas)
import Graphics.Babylon.TargetCamera (TargetCamera)
import Graphics.Babylon.Types (Mesh, Scene)
import Graphics.Canvas (CanvasElement)
import Math (max, min, pi)
import Prelude (negate, ($), (*), (+))

shadowMapSize :: Int
shadowMapSize = 4096

loadDistance :: Int
loadDistance = 4

unloadDistance :: Int
unloadDistance = 8

skyBoxRenderingGruop :: Int
skyBoxRenderingGruop = 0

terrainRenderingGroup :: Int
terrainRenderingGroup = 1

collesionEnabledRange :: Int
collesionEnabledRange = 1

enableWaterMaterial :: Boolean
enableWaterMaterial = false

initializeUI :: forall eff. Canvas -> CanvasElement -> Ref State -> Mesh -> TargetCamera -> Scene -> Materials -> Options -> Eff (Effects eff) Unit
initializeUI canvasGL canvas2d ref cursor camera scene materials options = do

    onRightMouseDrag \e -> do
        modifyRef ref \(State state) -> State state {
            cameraYaw = state.cameraYaw + toNumber e.movementX * options.cameraHorizontalSensitivity,
            cameraPitch = max (-pi * 0.45) $ min (pi * 0.45) $ state.cameraPitch + toNumber e.movementY * options.cameraVertialSensitivity
        }

    onWheel \e -> do
        modifyRef ref \(State state) -> State state {
            cameraRange = max options.cameraMinimumRange (min options.cameraMaximumRange (state.cameraRange + (toNumber e.deltaY * options.cameraZoomSpeed)))
        }


