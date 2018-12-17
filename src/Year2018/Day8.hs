{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Year2018.Day8 where

import Control.Monad (replicateM)
import Control.Monad.Trans.State (State, evalState, gets, modify)
import Data.Maybe (catMaybes, mapMaybe)
import Safe (atMay)
import Text.Megaparsec (sepEndBy)
import Text.Megaparsec.Char (space)
import Utils (integer, simpleParse)

data Tree a =
  Node [Tree a]
       [a]
  deriving (Show, Eq, Functor)

pop :: Int -> State [a] [a]
pop n = do
  result <- gets (take n)
  modify (drop n)
  pure result

buildTreeS :: State [Int] (Tree Int)
buildTreeS = do
  [x, y] <- pop 2
  children <- replicateM x buildTreeS
  metadata <- pop y
  pure $ Node children metadata

buildTree :: [Int] -> Tree Int
buildTree = evalState buildTreeS

------------------------------------------------------------
readSequence :: IO [Int]
readSequence = simpleParse "data/Year2018/Day8.txt" (integer `sepEndBy` space)

checksum :: Num a => Tree a -> a
checksum (Node xs ys) = sum (checksum <$> xs) + sum ys

value :: Tree Int -> Int
value (Node [] ys) = sum ys
value (Node xs ys) = sum $ value <$> catMaybes (lookupChild <$> ys)
  where
    lookupChild n = atMay xs (n - 1)

solution1 :: IO Int
solution1 = checksum . buildTree <$> readSequence

solution2 :: IO Int
solution2 = value . buildTree <$> readSequence
