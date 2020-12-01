{-# LANGUAGE ScopedTypeVariables #-}

module Year2020.Day1
  ( solution1
  , solution2
  ) where

import           Control.Monad        (guard)
import           Safe                 (headMay)
import           Text.Megaparsec      (sepEndBy)
import           Text.Megaparsec.Char (eol)
import           Utils                (integer, simpleParse)

readSequence :: IO [Int]
readSequence = simpleParse "data/Year2020/Day1.txt" (integer `sepEndBy` eol)

findPair :: [Int] -> Maybe Int
findPair xs = headMay $ do
  x <- xs
  y <- xs
  guard (x + y == 2020)
  pure (x * y)

solution1 :: IO (Maybe Int)
solution1 = findPair <$> readSequence

findTrio :: [Int] -> Maybe Int
findTrio xs = headMay $ do
  x <- xs
  y <- xs
  z <- xs
  guard (x + y + z == 2020)
  pure (x * y * z)

solution2 :: IO (Maybe Int)
solution2 = findTrio <$> readSequence
