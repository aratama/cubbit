module Game.Cubbit.Storage (saveChunk, listenAllChunks, getAllChunks) where

import Control.Monad.Eff (Eff)
import DOM (DOM)
import Data.Unit (Unit)
import Game.Cubbit.Chunk (Chunk)

foreign import saveChunk :: forall eff. Chunk -> Eff (dom :: DOM | eff) Unit

foreign import listenAllChunks :: forall eff. (Chunk -> Eff (dom :: DOM | eff) Unit) -> Eff (dom :: DOM | eff) Unit

foreign import getAllChunks :: forall eff. (Array Chunk -> Eff (dom :: DOM | eff) Unit) -> Eff (dom :: DOM | eff) Unit



