module Game.Cubbit.Generation where

import Game.Cubbit.BlockType (BlockTypes, blockTypes)
import Game.Cubbit.Chunk (Chunk)
import Game.Cubbit.ChunkIndex (ChunkIndex)

import PerlinNoise (Noise, createNoise, simplex2)

maxHeight :: Int
maxHeight = 25

terrainScale :: Number
terrainScale = 0.01

waterBlockHeight :: Int
waterBlockHeight = 3

createBlockMap :: ChunkIndex -> Int -> Chunk
createBlockMap index seed = createBlockMapJS (createNoise seed) simplex2 index terrainScale  waterBlockHeight maxHeight blockTypes

foreign import createBlockMapJS :: Noise -> (Number -> Number -> Noise -> Number) -> ChunkIndex -> Number -> Int ->Int -> BlockTypes -> Chunk
