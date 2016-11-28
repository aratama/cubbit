module Game.Cubbit.VertexDataPropsData (VertexDataPropsData(VertexDataPropsData)) where

import Graphics.Babylon.VertexData (VertexDataProps)

newtype VertexDataPropsData = VertexDataPropsData {
    standardMaterialBlocks :: VertexDataProps,
    waterMaterialBlocks :: VertexDataProps
}


