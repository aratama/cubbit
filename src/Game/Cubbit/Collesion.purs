module Game.Cubbit.Collesion (BodyTag, buildCollesionBoxes) where

import Control.Monad.Eff (Eff)
import Game.Cubbit.Chunk (ChunkWithMesh)
import Game.Cubbit.Constants (chunkSize)
import Game.Cubbit.LocalIndex (LocalIndex, localIndex)
import Graphics.Cannon.Type (Body, CANNON, World)

type BodyTag = String

type BuildCollesionBoxesImports = {
    chunkSize :: Int,
    localIndex :: (Int -> Int -> Int -> LocalIndex)
}

buildCollesionBoxes :: forall eff. ChunkWithMesh -> World BodyTag -> Eff (cannon :: CANNON | eff) (Array (Body BodyTag))
buildCollesionBoxes = _buildCollesionBoxes { chunkSize, localIndex }

foreign import _buildCollesionBoxes :: forall eff. BuildCollesionBoxesImports -> ChunkWithMesh -> World BodyTag -> Eff (cannon :: CANNON | eff) (Array (Body BodyTag))
