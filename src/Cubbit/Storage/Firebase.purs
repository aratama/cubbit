module Game.Cubbit.Firebase (saveChunkToFirebase, listenAllChunksFromForebase, listenOnceToTerrainAff, decode, listenToTerrain, createTerrainRef, listenToTerrain') where

import Control.Alternative (pure)
import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, error, errorShow)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Eff.Ref (REF)
import Data.Either (Either(..), either)
import Data.Foreign (Foreign, toForeign)
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.StrMap (StrMap, toList)
import Data.String.Unsafe (charCodeAt)
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import Data.Unit (Unit)
import Game.Cubbit.BoxelMap (BoxelMap)
import Game.Cubbit.Chunk (Chunk(..))
import Game.Cubbit.ChunkIndex (ChunkIndex)
import Game.Cubbit.Constants (chunkSize)
import LZString (decompressFromUTF16, compressToUTF16)
import Prelude (($), (*), (<>), (>>=), (#), bind)
import Unsafe.Coerce (unsafeCoerce)
import Web.Firebase (EventType(..), FIREBASE, Firebase, Reference, limitToLast, child, database, forEach, key, on, once, ref, set, val)

saveChunkToFirebase :: forall eff. Chunk -> Firebase -> Eff (firebase :: FIREBASE | eff) Unit
saveChunkToFirebase (Chunk chunk) firebase = do
    database firebase >>= ref "terrain" >>= child (unsafeCoerce chunk.index) >>= set (toForeign { blocks: compressToUTF16 $ boxelMapToString chunk.blocks })

listenAllChunksFromForebase :: forall eff. Firebase -> (Chunk -> Eff (firebase :: FIREBASE, console :: CONSOLE | eff) Unit) -> Eff (firebase :: FIREBASE, console :: CONSOLE | eff) Unit
listenAllChunksFromForebase firebase callback = do
    database firebase >>= ref "terrain" >>= once \snap -> do
        forEach snap \chunkSnap -> case decode (key chunkSnap) (val chunkSnap) of
            Left err -> error err
            Right chunk -> callback chunk

listenOnceToTerrain :: forall eff. Firebase
                -> (Error -> Eff (firebase :: FIREBASE, console :: CONSOLE, ref :: REF | eff) Unit)
                -> (List Chunk -> Eff (firebase :: FIREBASE, console :: CONSOLE, ref :: REF | eff) Unit)
                -> Eff (firebase :: FIREBASE, console :: CONSOLE, ref :: REF | eff) Unit
listenOnceToTerrain firebase reject resolve = database firebase >>= ref "terrain" >>= once \snap -> do
    let terrain = unsafeCoerce (val snap) :: StrMap Foreign
    either error resolve $ for (toList terrain) \(Tuple key value) -> decode key value

listenOnceToTerrainAff :: forall eff. Firebase -> Aff (firebase :: FIREBASE, console :: CONSOLE, ref :: REF | eff) (List Chunk)
listenOnceToTerrainAff firebase = makeAff $ listenOnceToTerrain firebase



listenToTerrain :: forall eff. Firebase
                -> (Chunk -> Eff (firebase :: FIREBASE, console :: CONSOLE, ref :: REF | eff) Unit)
                -> Eff (firebase :: FIREBASE, console :: CONSOLE, ref :: REF | eff) Reference
listenToTerrain firebase resolve = do
    ref <- database firebase >>= ref "terrain"
    limitToLast 1 ref >>= on ChildAdded errorShow \snap -> either error resolve $ decode (key snap) (val snap)
    ref # on ChildChanged errorShow \snap -> either error resolve $ decode (key snap) (val snap)
    pure ref


createTerrainRef :: forall eff. Firebase
                -> Eff (firebase :: FIREBASE, console :: CONSOLE, ref :: REF | eff) Reference
createTerrainRef firebase = database firebase >>= ref "terrain"

listenToTerrain' :: forall eff. Reference
                -> (Chunk -> Eff (firebase :: FIREBASE, console :: CONSOLE, ref :: REF | eff) Unit)
                -> Eff (firebase :: FIREBASE, console :: CONSOLE, ref :: REF | eff) Unit
listenToTerrain' ref resolve = do
    limitToLast 1 ref >>= on ChildAdded errorShow \snap -> either error resolve $ decode (key snap) (val snap)
    ref # on ChildChanged errorShow \snap -> either error resolve $ decode (key snap) (val snap)












decode :: String -> Foreign -> Either String Chunk
decode key value = do
    let chunk = unsafeCoerce value :: { blocks :: String }
    let decompressed = decompressFromUTF16 chunk.blocks
    case decompressed of
        Nothing -> Left ("invalid chunk data at " <> key)
        Just decompressed' -> pure $ Chunk {
            index: parseIndex key,
            blocks: from (chunkSize * chunkSize * chunkSize) (\i -> charCodeAt i decompressed')
        }

foreign import boxelMapToString :: BoxelMap -> String

foreign import from :: Int -> (Int -> Int) -> BoxelMap

foreign import parseIndex :: String -> ChunkIndex

