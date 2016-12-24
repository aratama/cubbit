module Game.Cubbit.Storage where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, error)
import DOM (DOM)
import Data.ArrayBuffer.Types (Uint8Array)
import Data.Foreign (toForeign)
import Data.Maybe (Maybe(..))
import Data.String.Unsafe (charCodeAt)
import Data.Unit (Unit)
import Game.Cubbit.BoxelMap (BoxelMap)
import Game.Cubbit.Chunk (Chunk(..))
import Game.Cubbit.ChunkIndex (ChunkIndex)
import LZString (decompressFromUTF16, compressToUTF16)
import Prelude ((#), ($), bind, (>>=), (<>))
import Unsafe.Coerce (unsafeCoerce)
import Web.Firebase (FIREBASE, Firebase, child, database, forEach, once, ref, val, key, set)

foreign import saveChunk :: forall eff. Chunk -> Eff (dom :: DOM | eff) Unit

foreign import listenAllChunks :: forall eff. (Chunk -> Eff (dom :: DOM | eff) Unit) -> Eff (dom :: DOM | eff) Unit

saveChunkToFirebase :: forall eff. Chunk -> Firebase -> Eff (firebase :: FIREBASE | eff) Unit
saveChunkToFirebase (Chunk chunk) firebase = do
    let compressed = compressToUTF16 $ boxelMapToString chunk.blocks
    r <- database firebase >>= ref "terrain" >>= child (unsafeCoerce chunk.index)
    set (toForeign { blocks: compressed }) r

listenAllChunksFromForebase :: forall eff. Firebase -> (Chunk -> Eff (firebase :: FIREBASE, console :: CONSOLE | eff) Unit) -> Eff (firebase :: FIREBASE, console :: CONSOLE | eff) Unit
listenAllChunksFromForebase firebase callback = do
    ref <- database firebase >>= ref "terrain"
    ref # once \snap -> do
        forEach snap \chunkSnap -> do
            let value = val chunkSnap
            let decompressed = decompressFromUTF16 (unsafeCoerce value).blocks
            case decompressed of
                Nothing -> error ("invalid chunk data at "  <> key chunkSnap)
                Just decompressed' -> do
                    array <- from 4096 (\i -> charCodeAt i decompressed')
                    callback $ Chunk {
                        index: parseIndex $ key chunkSnap,
                        blocks: unsafeCoerce array
                    }

foreign import boxelMapToString :: forall a. BoxelMap a -> String

foreign import from :: forall eff. Int -> (Int -> Int) -> Eff (firebase :: FIREBASE | eff) Uint8Array

foreign import parseIndex :: String -> ChunkIndex

