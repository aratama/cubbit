module Game.Cubbit.UI (initializeUI) where

import Control.Alternative (pure)
import Control.Bind (bind, (>>=))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (Ref, modifyRef, readRef)
import Data.BooleanAlgebra (not)
import Data.Int (toNumber)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Unit (Unit, unit)
import Game.Cubbit.BlockType (dirtBlock, airBlock)
import Game.Cubbit.Event (onButtonClick, onMouseClick, onMouseMove, onRightMouseDrag, onWheel)
import Game.Cubbit.MeshBuilder (editBlock)
import Game.Cubbit.Types (Effects, Materials, Mode(..), State(State), Options)
import Game.Cubbit.Update (pickBlock)
import Graphics.Babylon (Canvas)
import Graphics.Babylon.DebugLayer (show, hide) as DebugLayer
import Graphics.Babylon.Scene (getDebugLayer)
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

    onMouseMove \e -> do
        modifyRef ref \(State s) -> State s {
            mousePosition = {
                x: e.offsetX,
                y: e.offsetY
            }
        }

    onButtonClick "debuglayer" do
        modifyRef ref (\(State state) -> State state { debugLayer = not state.debugLayer })
        State state <- readRef ref
        if state.debugLayer
            then getDebugLayer scene >>= DebugLayer.show true true Nothing
            else getDebugLayer scene >>= DebugLayer.hide

    onMouseClick \e -> do

        State state <- readRef ref

        let put block = do
                picked <- pickBlock scene cursor (State state) state.mousePosition.x state.mousePosition.y
                case picked of
                    Nothing -> pure unit
                    Just blockIndex -> editBlock ref materials scene blockIndex block

        case state.mode of
            Put -> put dirtBlock
            Remove -> put airBlock
            Move -> pure unit


    onRightMouseDrag \e -> do
        modifyRef ref \(State state) -> State state {
            cameraYaw = state.cameraYaw + toNumber e.movementX * options.cameraHorizontalSensitivity,
            cameraPitch = max (-pi * 0.45) $ min (pi * 0.45) $ state.cameraPitch + toNumber e.movementY * options.cameraVertialSensitivity
        }

    onWheel \e -> do
        modifyRef ref \(State state) -> State state {
            cameraRange = max options.cameraMinimumRange (min options.cameraMaximumRange (state.cameraRange + (toNumber e.deltaY * options.cameraZoomSpeed)))
        }


