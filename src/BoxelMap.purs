module Graphics.Babylon.Example.Sandbox.BoxelMap (BoxelMap, empty, lookup, insert, delete) where

import Data.Functor (class Functor)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Graphics.Babylon.Example.Sandbox.LocalIndex (LocalIndex)

foreign import data BoxelMap :: * -> *

instance functor_ShowMap :: Functor BoxelMap where
    map = mapBoxelMap

foreign import mapBoxelMap :: forall a b. (a -> b) -> BoxelMap a -> BoxelMap b

foreign import _lookup :: forall a. LocalIndex -> BoxelMap a -> Nullable a

lookup :: forall a. LocalIndex -> BoxelMap a -> Maybe a
lookup key map = toMaybe (_lookup key map)

foreign import insert :: forall a. LocalIndex -> a -> BoxelMap a -> BoxelMap a

foreign import delete :: forall a. LocalIndex -> BoxelMap a -> BoxelMap a

foreign import empty :: forall a. BoxelMap a

