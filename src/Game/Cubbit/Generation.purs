module Game.Cubbit.Generation (createBlockMap) where

import Game.Cubbit.BlockType (BlockTypes, blockTypes)
import Game.Cubbit.Chunk (Chunk)
import Game.Cubbit.ChunkIndex (ChunkIndex, runChunkIndex)
import Game.Cubbit.Constants (chunkSize)
import PerlinNoise (Noise, simplex2)

type GenerateReferences = {
    chunkSize :: Int,
    terrainScale :: Number,
    waterBlockHeight :: Int,
    maxHeight :: Int,
    blockTypes :: BlockTypes,
    simplex2 :: Number -> Number -> Noise -> Number,
    runChunkIndex :: ChunkIndex -> { x :: Int, y :: Int, z :: Int }
}

generateReferences :: GenerateReferences
generateReferences = {
    chunkSize: chunkSize,
    terrainScale: 0.01,
    waterBlockHeight: 3,
    maxHeight: 15,
    blockTypes: blockTypes,
    simplex2: simplex2,
    runChunkIndex: runChunkIndex
}

foreign import _createBlockMapJS :: GenerateReferences -> Noise -> ChunkIndex -> Chunk

createBlockMap :: Noise -> ChunkIndex -> Chunk
createBlockMap = _createBlockMapJS generateReferences