module Game.Cubbit.AABB where

import Game.Cubbit.Vec (Vec)

type AABB = { position :: Vec, size :: Vec }


moveWithCollesion :: Array AABB -> AABB -> Vec -> AABB
moveWithCollesion env aabb velocity = aabb


