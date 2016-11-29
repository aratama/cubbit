module Game.Cubbit.Constants where

import Prelude ((+), (*))
import Data.Int (pow)

chunkSize :: Int
chunkSize = 16

loadDistance :: Int
loadDistance = 8

unloadDistance :: Int
unloadDistance = loadDistance + 2

fogDensity :: Number
fogDensity = 0.005



maximumLoadedChunks :: Int
maximumLoadedChunks = pow (1 + loadDistance * 2) 3 + 1000



shadowDisplayRange :: Int 
shadowDisplayRange = 7