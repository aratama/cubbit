module Graphics.Babylon.Example.Sandbox.AABB where

import Graphics.Babylon.Example.Sandbox.Vec (Vec)

type AABB = { position :: Vec, size :: Vec }


moveWithCollesion :: Array AABB -> AABB -> Vec -> AABB
moveWithCollesion env aabb velocity = aabb


