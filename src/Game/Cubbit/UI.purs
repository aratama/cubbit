module Game.Cubbit.UI (initializeUI) where

import Control.Alt (void)
import Control.Alternative (pure)
import Control.Bind (bind, (>>=))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (Ref, modifyRef, readRef)
import Data.BooleanAlgebra (not)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Unit (Unit, unit)
import Game.Cubbit.BlockType (dirtBlock, airBlock)
import Game.Cubbit.Event (onButtonClick, onMouseClick, onMouseMove)
import Game.Cubbit.MeshBuilder (editBlock)
import Game.Cubbit.Types (Effects, Mode(..), State(State), Materials)
import Game.Cubbit.Update (pickBlock)
import Graphics.Babylon (Canvas)
import Graphics.Babylon.AbstractMesh (setIsVisible)
import Graphics.Babylon.DebugLayer (show, hide) as DebugLayer
import Graphics.Babylon.FreeCamera (FreeCamera, freeCameraToCamera)
import Graphics.Babylon.Mesh (meshToAbstractMesh)
import Graphics.Babylon.Scene (getDebugLayer, setActiveCameras)
import Graphics.Babylon.TargetCamera (TargetCamera, targetCameraToCamera)
import Graphics.Babylon.Types (Mesh, Scene)
import Graphics.Canvas (CanvasElement)
import Prelude (($))

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

initializeUI :: forall eff. Canvas -> CanvasElement -> Ref State -> Mesh -> FreeCamera -> TargetCamera -> Scene -> Materials -> Eff (Effects eff) Unit
initializeUI canvasGL canvas2d ref cursor camera miniMapCamera scene materials = do


    onMouseMove \e -> do
        modifyRef ref \(State s) -> State s {
            mousePosition = {
                x: e.offsetX,
                y: e.offsetY
            }
        }

    let prepareModeButton id value = onButtonClick id do
            modifyRef ref (\(State state) -> State state { mode = value })
            setIsVisible (case value of
                Put -> true
                Remove -> true
                Move -> false) (meshToAbstractMesh cursor)


    prepareModeButton "move" Move
    prepareModeButton "add" Put
    prepareModeButton "remove" Remove

    onButtonClick "position" $ void do
        modifyRef ref \(State state) -> State state {
            position = { x: 0.0, y: 30.0, z: 0.0 }
        }

    onButtonClick "minimap" do
        modifyRef ref (\(State state) -> State state { minimap = not state.minimap })
        State state <- readRef ref
        if state.minimap
            then setActiveCameras [freeCameraToCamera camera, targetCameraToCamera miniMapCamera] scene
            else setActiveCameras [freeCameraToCamera camera] scene

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






