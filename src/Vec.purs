module Graphics.Babylon.Example.Sandbox.Vec where

import Prelude ((&&), (==))

type Vec = { x :: Number, y :: Number, z :: Number }

vec :: Number -> Number -> Number -> { x :: Number, y :: Number, z :: Number }
vec x y z = { x, y, z }

vecEq :: Vec -> Vec -> Boolean
vecEq v r = v.x == r.x && v.y == r.y && v.z == r.z

