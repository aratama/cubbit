module Data.ShowMap (ShowMap, lookup, member, insert, delete, fromFoldable, toList, empty, size, fromStrMap, isEmpty) where

import Data.Foldable (class Foldable, foldMap, foldl, foldr)
import Data.Functor (class Functor, map)
import Data.Int (floor)
import Data.List (List)
import Data.Maybe (Maybe)
import Data.Monoid (class Monoid, mempty)
import Data.Semigroup (class Semigroup, append)
import Data.Show (class Show, show)
import Data.StrMap (StrMap, delete, empty, insert, lookup, member, toList, size, isEmpty) as StrMap
import Data.Traversable (class Traversable, sequence, traverse)
import Data.Tuple (Tuple(..))

newtype ShowMap k a = ShowMap (StrMap.StrMap a)

instance semigroup_ShowMap :: (Semigroup a) => Semigroup (ShowMap k a) where
    append (ShowMap a) (ShowMap b) = ShowMap (append a b)

instance monoid_ShowMap :: (Monoid a) => Monoid (ShowMap k a) where
    mempty = ShowMap mempty

instance foldable_ShowMap :: Foldable (ShowMap k) where
    foldMap f (ShowMap m) = foldMap f m
    foldl f a (ShowMap m) = foldl f a m
    foldr f a (ShowMap m) = foldr f a m

instance functor_ShowMap :: Functor (ShowMap k) where
    map f (ShowMap m) = ShowMap (map f m)

instance traversable_ShowMap :: Traversable (ShowMap k) where
    traverse f (ShowMap m) = map ShowMap (traverse f m)
    sequence (ShowMap m) = map ShowMap (sequence m)

--traverse :: forall a b m. Applicative m => (a -> m b) -> StrMap a -> m (StrMap b)

lookup :: forall k a. (Show k) => k -> ShowMap k a -> Maybe a
lookup k (ShowMap m) = StrMap.lookup (show k) m

member :: forall k a. (Show k) => k -> ShowMap k a -> Boolean
member k (ShowMap m) = StrMap.member (show k) m

insert :: forall k a. (Show k) => k -> a -> ShowMap k a -> ShowMap k a
insert k v (ShowMap m) = ShowMap (StrMap.insert (show k) v m)

delete :: forall k a. (Show k) => k -> ShowMap k a -> ShowMap k a
delete k (ShowMap m) = ShowMap (StrMap.delete (show k) m)

empty :: forall k a. ShowMap k a
empty = ShowMap StrMap.empty

fromFoldable :: forall f k a. (Show k, Foldable f) => f (Tuple k a) -> ShowMap k a
fromFoldable f = foldl (\m (Tuple k v) -> insert k v m) empty f

fromList :: forall k a. (Show k) => List (Tuple k a) -> ShowMap k a
fromList f = foldl (\m (Tuple k v) -> insert k v m) empty f

toList :: forall k a. ShowMap k a -> List (Tuple String a)
toList (ShowMap m) = StrMap.toList m

size :: forall k a. ShowMap k a -> Int
size (ShowMap m) = floor (StrMap.size m)

fromStrMap :: forall k a. StrMap.StrMap a -> ShowMap k a
fromStrMap m = ShowMap m

isEmpty :: forall k a. ShowMap k a -> Boolean
isEmpty (ShowMap m) = StrMap.isEmpty m
