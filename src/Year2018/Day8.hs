{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Year2018.Day8 where

import Control.Monad (replicateM)
import Control.Monad.Trans.State (State, evalState, gets, put)
import Data.Functor.Foldable
  ( Base,
    Corecursive,
    Recursive (project),
    cata,
    embed,
  )
import Data.Maybe (fromMaybe)
import Safe (atMay)
import Text.Megaparsec (sepEndBy)
import Text.Megaparsec.Char (space)
import Utils (integer, simpleParse)

data Tree a
  = Node [a] [Tree a]
  deriving (Show, Eq, Functor)

------------------------------------------------------------

-- | TreeF is just Tree with 1) the recursion replaced with a type
-- variable and 2) the `List` of children replaced with an arbitrary
-- container of children.
data TreeF a f r
  = NodeF a (f r)
  deriving (Show, Eq, Functor)

type instance Base (Tree a) = TreeF [a] []

instance Recursive (Tree a) where
  project (Node metadata children) = NodeF metadata children

instance Corecursive (Tree a) where
  embed (NodeF metadata children) = Node metadata children

------------------------------------------------------------
pop :: Int -> State [a] [a]
pop n = do
  (result, remainder) <- gets (splitAt n)
  put remainder
  pure result

buildTreeS :: State [Int] (Tree Int)
buildTreeS = do
  ~[x, y] <- pop 2
  children <- replicateM x buildTreeS
  metadata <- pop y
  pure $ Node metadata children

buildTree :: [Int] -> Tree Int
buildTree = evalState buildTreeS

------------------------------------------------------------
readSequence :: IO [Int]
readSequence = simpleParse "data/Year2018/Day8.txt" (integer `sepEndBy` space)

-- | The checksum is just the sum of the children plus the sum of this
-- node's metadata. It's quite neat how using recursion schemes
-- makes this abundantly clear.
checksum :: Num a => Tree a -> a
checksum = cata $ \(NodeF metadata children) -> sum metadata + sum children

value :: Tree Int -> Int
value = cata alg
  where
    alg (NodeF metadata []) = sum metadata
    alg (NodeF metadata children) = sum $ valueAtIndex <$> metadata
      where
        valueAtIndex n = fromMaybe 0 $ atMay children (n - 1)

solution1 :: IO Int
solution1 = checksum . buildTree <$> readSequence

solution2 :: IO Int
solution2 = value . buildTree <$> readSequence
