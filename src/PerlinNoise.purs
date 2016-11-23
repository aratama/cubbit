module PerlinNoise where

foreign import data Noise :: *

foreign import createNoise :: Int -> Noise

foreign import simplex2 :: Number -> Number -> Noise -> Number

foreign import simplex3 :: Number -> Number -> Number -> Noise -> Number
