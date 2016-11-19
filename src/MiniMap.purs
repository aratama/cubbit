module Graphics.Babylon.Example.Sandbox.MiniMap where

import Control.Alt (void)
import Control.Bind (bind)
import Control.Monad.Eff (Eff)
import Data.Foldable (for_)
import Data.Int (toNumber) as Int
import Data.Ring (negate)
import Data.Show (show)
import Data.Unit (Unit)
import Graphics.Babylon.Example.Sandbox.Chunk (Chunk(..))
import Graphics.Babylon.Example.Sandbox.ChunkIndex (runChunkIndex)
import Graphics.Babylon.Example.Sandbox.Constants (chunkSize)
import Graphics.Babylon.Example.Sandbox.Terrain (Terrain, getChunkMap, globalPositionToChunkIndex)
import Graphics.Babylon.Example.Sandbox.Types (Effects)
import Graphics.Babylon.Example.Sandbox.Vec (Vec)
import Graphics.Canvas (CanvasElement, arc, clearRect, closePath, fillPath, fillRect, fillText, getContext2D, lineTo, rotate, setFillStyle, setStrokeStyle, strokeRect, translate, withContext)
import Prelude ((*), (-), (<>))

renderMiniMap :: forall eff. Terrain -> Vec -> Vec -> CanvasElement -> Eff (Effects eff) Unit
renderMiniMap terrain pos cameraRot minimap = void do

    mapContext <- getContext2D minimap

    let i = runChunkIndex (globalPositionToChunkIndex pos.x pos.y pos.z)

    clearRect mapContext { x: 0.0, y: 0.0, w: 1280.0, h:720.0 }

    withContext mapContext do
        translate { translateX: 200.0, translateY: 150.0 } mapContext

        rotate ( - cameraRot.y) mapContext

        translate { translateX: -pos.x, translateY: pos.z } mapContext

        setFillStyle "rgba(0, 200, 0, 0.3)" mapContext
        for_ (getChunkMap terrain) \(dat@{ blocks: Chunk chunk }) -> do
            let index = runChunkIndex chunk.index
            fillRect mapContext {
                x: Int.toNumber (chunkSize * index.x),
                y: Int.toNumber (chunkSize * negate index.z),
                w: Int.toNumber chunkSize,
                h: Int.toNumber chunkSize
            }

        -- x axis
        setStrokeStyle "rgba(255, 0, 0, 0.5)" mapContext
        strokeRect mapContext { x: 0.0, y: 0.0, w: 2000.0, h: 1.0 }

        setStrokeStyle "rgba(200, 200, 200, 0.5)" mapContext
        strokeRect mapContext { x: negate 2000.0, y: 0.0, w: 2000.0, h: 1.0 }

        -- z axis
        setStrokeStyle "rgba(0, 0, 255, 0.5)" mapContext
        strokeRect mapContext { x: 0.0, y: negate 2000.0, w: 1.0, h: 2000.0 }

        setStrokeStyle "rgba(200, 200, 200, 0.5)" mapContext
        strokeRect mapContext { x: 0.0, y: 0.0, w: 1.0, h: 2000.0 }

        translate { translateX: pos.x, translateY: -pos.z } mapContext

        -- position
        rotate (cameraRot.y - 3.14159265 * 0.5) mapContext
        setFillStyle "rgba(255, 255, 0, 0.5)" mapContext
        fillPath mapContext do
            arc mapContext { x: 0.0, y: 0.0,  r: 20.0, start: -0.5, end: 0.5 }
            lineTo mapContext 0.0 0.0
            closePath mapContext


        setFillStyle "orange" mapContext
        fillRect mapContext { x: -3.0, y: -3.0, w: 6.0, h: 6.0 }





    --putImageData context imageMap 0.0 0.0
    setFillStyle "white" mapContext
    fillText mapContext (" x: " <> show pos.x) 10.0 20.0
    fillText mapContext (" y: " <> show pos.y) 10.0 40.0
    fillText mapContext (" z: " <> show pos.z) 10.0 60.0

    fillText mapContext ("ix: " <> show i.x) 10.0 80.0
    fillText mapContext ("iy: " <> show i.y) 10.0 100.0
    fillText mapContext ("iz: " <> show i.z) 10.0 120.0


