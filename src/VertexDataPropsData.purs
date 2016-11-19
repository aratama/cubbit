module Graphics.Babylon.Example.Sandbox.VertexDataPropsData (VertexDataPropsData(VertexDataPropsData)) where

import Control.Bind (bind)
import Data.Foreign (toForeign)
import Data.Foreign.Class (readProp, write, class AsForeign, class IsForeign)
import Prelude (pure, ($))

import Graphics.Babylon.VertexData (VertexDataProps)
import Graphics.Babylon.Example.Sandbox.Chunk (Chunk)

newtype VertexDataPropsData = VertexDataPropsData {
    terrain :: Chunk,
    standardMaterialBlocks :: VertexDataProps
}

instance isForeign_VertexDataPropsData :: IsForeign VertexDataPropsData where
    read value = do
        terrain <- readProp "terrain" value
        standardMaterialBlocks <- readProp "standardMaterialBlocks" value
        pure $ VertexDataPropsData { terrain, standardMaterialBlocks }

instance asForeign_VertexDataPropsData :: AsForeign VertexDataPropsData where
    write (VertexDataPropsData value) = toForeign {
        terrain: write value.terrain,
        standardMaterialBlocks: write value.standardMaterialBlocks
    }

