{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}

module GraphUtils where

import Control.Newtype.Generics (Newtype, unpack)
import Data.Functor.Foldable (Base, Corecursive, ListF(Cons, Nil), embed, refix)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic)

newtype Adjacency a =
  Adjacency (Map a (Set a))
  deriving (Show, Eq, Generic)

instance Newtype (Adjacency a)

instance Ord a => Monoid (Adjacency a) where
  mempty = Adjacency Map.empty

instance Ord a => Semigroup (Adjacency a) where
  (Adjacency x) <> (Adjacency y) = Adjacency $ Map.unionWith Set.union x y

------------------------------------------------------------
type instance Base (Adjacency a) = ListF (a, a)

instance Ord a => Corecursive (Adjacency a) where
  embed Nil = mempty
  embed (Cons (k, v) rest) =
    singleton k (Set.singleton v) <> singleton v mempty <> rest

------------------------------------------------------------
singleton :: a -> Set a -> Adjacency a
singleton k vs = Adjacency $ Map.singleton k vs

toGraph' :: Ord a => [(a, a)] -> Adjacency a
toGraph' = refix

-- | I want to create an adjacency list from a list of pairs.
-- Honestly, I'm not sure if doing it via recursion schemes is clever
-- or just 'too clever', but I'm having fun. :-)
toGraph :: Ord a => [(a, a)] -> Map a (Set a)
toGraph = unpack . toGraph'
