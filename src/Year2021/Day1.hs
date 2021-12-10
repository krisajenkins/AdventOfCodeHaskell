{-# LANGUAGE ScopedTypeVariables #-}

module Year2021.Day1
  ( solution1,
    solution2,
  )
where

import Text.Megaparsec (sepEndBy)
import Text.Megaparsec.Char (eol)
import Utils (integer, simpleParse)

readSequence :: IO [Int]
readSequence = simpleParse "data/Year2021/Day1.txt" (integer `sepEndBy` eol)

solution1 :: IO Int
solution1 = do
  depths <- readSequence
  pure $ countTrue $ zipWith (<) depths (tail depths)

solution2 :: IO Int
solution2 = do
  depths <- readSequence
  let windows :: [Int]
      windows = zipWith3 (\a b c -> a + b + c) depths (tail depths) (tail (tail depths))
  pure $ countTrue $ zipWith (<) windows (tail windows)

countTrue :: [Bool] -> Int
countTrue = length . filter id
