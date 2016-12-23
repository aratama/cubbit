module Game.Cubbit.Storage where

import Control.Monad.Eff (Eff)
import DOM (DOM)
import Data.Unit (Unit)
import Game.Cubbit.Chunk (Chunk(..))
import Game.Cubbit.ChunkIndex (ChunkIndex)

foreign import saveChunk :: forall eff. Chunk -> Eff (dom :: DOM | eff) Unit

foreign import listenAllChunks :: forall eff. (Chunk -> Eff (dom :: DOM | eff) Unit) -> Eff (dom :: DOM | eff) Unit


foreign import saveChunkToFirebase :: forall eff. Chunk -> Eff (dom :: DOM | eff) Unit

foreign import listenAllChunksFromForebase :: forall eff. (Chunk -> Eff (dom :: DOM | eff) Unit) -> Eff (dom :: DOM | eff) Unit
