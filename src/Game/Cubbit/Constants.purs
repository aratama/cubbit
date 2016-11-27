module Game.Cubbit.Constants where

import Prelude ((+))

chunkSize :: Int
chunkSize = 16

loadDistance :: Int
loadDistance = 8

unloadDistance :: Int
unloadDistance = loadDistance + 2

-- fog
fogDensity :: Number
fogDensity = 0.005

fogStart :: Number
fogStart = 600.0

fogEnd :: Number
fogEnd = 1000.0