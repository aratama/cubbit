module Game.Cubbit.Terrain (
 Terrain(..), createTerrain,
 globalPositionToChunkIndex, globalPositionToLocalIndex, globalPositionToGlobalIndex, globalIndexToChunkIndex, globalIndexToLocalIndex,
 lookupSolidBlockByVec, lookupSolidBlock, insertChunk, lookupChunk, chunkCount, getChunkMap, lookupBlockByVec, isSolidBlock
) where

import Control.Bind (bind, pure)
import Control.Monad.Eff (Eff)
import Data.EuclideanRing (mod)
import Data.Int (floor, toNumber)
import Data.Map (Map, empty)
import Data.Maybe (Maybe(..))
import Data.Unit (Unit)
import Game.Cubbit.BlockIndex (BlockIndex, blockIndex, runBlockIndex)
import Game.Cubbit.BlockType (BlockType, airBlock, bushBlock, leavesBlock)
import Game.Cubbit.BoxelMap (lookup) as Boxel
import Game.Cubbit.ChunkInstance (ChunkInstance)
import Game.Cubbit.ChunkIndex (ChunkIndex, chunkIndex, runChunkIndex)
import Game.Cubbit.ChunkMap (ChunkMap, createChunkMap, insert, lookup, size)
import Game.Cubbit.Constants (chunkSize)
import Game.Cubbit.LocalIndex (LocalIndex, localIndex)
import Game.Cubbit.Vec (Vec)
import Graphics.Cannon.Type (Body)
import PerlinNoise (Noise, createNoise)
import Prelude ((*), (+), (-), (/), (==), (/=), (&&))

newtype Terrain = Terrain {
    map :: ChunkMap,
    noise :: Noise,
    bodies :: Map ChunkIndex (Array Body)
}

getChunkMap :: Terrain -> ChunkMap
getChunkMap (Terrain t) = t.map

createTerrain :: forall eff. Int -> Eff eff Terrain
createTerrain seed = do
    map <- createChunkMap
    pure (Terrain { map, noise: createNoise seed, bodies: empty })

chunkCount :: forall eff. Terrain -> Eff eff Int
chunkCount (Terrain t) = size t.map

globalPositionToChunkIndex :: Number -> Number -> Number -> ChunkIndex
globalPositionToChunkIndex x y z = chunkIndex (f x) (f y) (f z)
  where
    f v = floor (v + 1000000.0 * toNumber chunkSize) / chunkSize - 1000000

globalPositionToLocalIndex :: Number -> Number -> Number -> BlockIndex
globalPositionToLocalIndex x y z = blockIndex (f x) (f y) (f z)
  where
    delta = toNumber chunkSize * 1000000.0
    f v = mod (floor (v + delta)) chunkSize

globalPositionToGlobalIndex :: Number -> Number -> Number -> BlockIndex
globalPositionToGlobalIndex x y z = blockIndex (f x) (f y) (f z)
  where
    f v = floor (v + 1000000.0) - 1000000

globalIndexToChunkIndex :: BlockIndex -> ChunkIndex
globalIndexToChunkIndex b = chunkIndex (f bi.x) (f bi.y) (f bi.z)
  where
    bi = runBlockIndex b
    f v = (v + 1000000 * chunkSize) / chunkSize - 1000000



lookupChunk :: forall eff. ChunkIndex -> Terrain -> Eff eff (Maybe ChunkInstance)
lookupChunk index (Terrain terrain) = lookup index terrain.map

lookupSolidBlockByVec :: forall eff. Vec -> Terrain -> Eff eff (Maybe BlockType)
lookupSolidBlockByVec p (Terrain terrain) = lookupSolidBlock (globalPositionToGlobalIndex p.x p.y p.z) (Terrain terrain)

lookupSolidBlock :: forall eff. BlockIndex -> Terrain -> Eff eff (Maybe BlockType)
lookupSolidBlock globalIndex (Terrain terrain) = do
        let chunkIndex = globalIndexToChunkIndex globalIndex
        let localIndex = globalIndexToLocalIndex globalIndex
        chunkMaybe <- lookup chunkIndex terrain.map
        case chunkMaybe of
            Just { blocks } -> case  Boxel.lookup localIndex blocks of
                Just blockType -> pure if blockType == airBlock then Nothing else Just blockType
                _ -> pure Nothing
            _ -> pure Nothing


isSolidBlock :: BlockType -> Boolean
isSolidBlock block = block /= airBlock && block /= bushBlock && block /= leavesBlock

lookupBlockByVec :: forall eff. Vec -> Terrain -> Eff eff (Maybe BlockType)
lookupBlockByVec p (Terrain terrain) = lookupBlock (globalPositionToGlobalIndex p.x p.y p.z) (Terrain terrain)

lookupBlock :: forall eff. BlockIndex -> Terrain -> Eff eff (Maybe BlockType)
lookupBlock globalIndex (Terrain terrain) = do
        let chunkIndex = globalIndexToChunkIndex globalIndex
        let localIndex = globalIndexToLocalIndex globalIndex
        chunkMaybe <- lookup chunkIndex terrain.map
        case chunkMaybe of
            Just { blocks } -> pure (Boxel.lookup localIndex blocks)
            _ -> pure Nothing

globalIndexToLocalIndex :: BlockIndex -> LocalIndex
globalIndexToLocalIndex index = localIndex cx cy cz
  where
    chunkIndex = runChunkIndex (globalIndexToChunkIndex index)
    globalIndex = runBlockIndex index
    cx = globalIndex.x - chunkSize * chunkIndex.x
    cy = globalIndex.y - chunkSize * chunkIndex.y
    cz = globalIndex.z - chunkSize * chunkIndex.z

insertChunk :: forall eff. ChunkInstance -> Terrain -> Eff eff Unit
insertChunk cmesh (Terrain terrain) = insert cmesh.index cmesh terrain.map


