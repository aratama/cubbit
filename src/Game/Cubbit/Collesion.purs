module Game.Cubbit.Collesion (BodyTag, buildCollesionBoxes) where

import Control.Monad.Eff (Eff)
import Game.Cubbit.Chunk (ChunkWithMesh)
import Game.Cubbit.Terrain (Terrain(..))
import Graphics.Cannon.Type (Body, CANNON, World)
import Prelude ((<=), (&&), (-), (+), pure, bind)

newtype AABB = AABB {
    x :: Number,
    y :: Number,
    z :: Number,
    w :: Number,
    h :: Number,
    d :: Number
}

intersect :: AABB -> AABB -> Boolean
intersect (AABB a) (AABB b) = a.x - a.w <= b.x + b.w && b.x - a.w <= a.x + a.w &&
                              a.y - a.h <= b.y + b.h && b.y - a.h <= a.y + a.h &&
                              a.z - a.d <= b.z + b.d && b.z - a.d <= a.z + a.d

move :: forall eff. AABB -> Number -> Number -> Number -> (Int -> Int -> Int -> Eff eff Boolean) -> Eff eff AABB
move aabb dx dy dz map = do
    aabb'   <- move' aabb   dx 0.0 0.0
    aabb''  <- move' aabb'  0.0 dy 0.0
    aabb''' <- move' aabb'' 0.0 0.0 dz
    pure aabb'''
  where
    move' a dx' dy' dz' = pure a

type BodyTag = String

foreign import buildCollesionBoxes :: forall eff. ChunkWithMesh -> World BodyTag -> Eff (cannon :: CANNON | eff) (Array (Body BodyTag))
