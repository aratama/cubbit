module Graphics.Babylon.Example.Sandbox.Generation where

import Graphics.Babylon.Example.Sandbox.BlockType (BlockTypes, blockTypes)
import Graphics.Babylon.Example.Sandbox.Chunk (Chunk)
import Graphics.Babylon.Example.Sandbox.ChunkIndex (ChunkIndex)

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
