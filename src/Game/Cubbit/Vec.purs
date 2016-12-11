module Game.Cubbit.Vec where

import Math (cos, sin)
import Prelude ((&&), (==), (-), (+), (*))

type Vec = { x :: Number, y :: Number, z :: Number }

vec :: Number -> Number -> Number -> { x :: Number, y :: Number, z :: Number }
vec x y z = { x, y, z }

vecZero :: Vec
vecZero = vec 0.0 0.0 0.0

vecEq :: Vec -> Vec -> Boolean
vecEq v r = v.x == r.x && v.y == r.y && v.z == r.z

vecRotY :: Number -> Vec -> Vec
vecRotY rot v = {
    x: cos rot * v.x - sin rot * v.z,
    y: v.y,
    z: sin rot * v.x + cos rot * v.z
}

vecAdd :: Vec -> Vec -> Vec
vecAdd v r = {
    x: v.x + r.x,
    y: v.y + r.y,
    z: v.z + r.z
}
