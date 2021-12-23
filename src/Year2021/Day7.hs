{-# LANGUAGE ScopedTypeVariables #-}

module Year2021.Day7
  ( solution1,
    solution2,
    calculateBestDistance,
    sumToN,
  )
where

import Text.Megaparsec (sepEndBy)
import Text.Megaparsec.Char (char)
import Utils (integer, simpleParse)

readSequence :: IO [Int]
readSequence = simpleParse "data/Year2021/Day7.txt" (integer `sepEndBy` char ',')

calculateBestDistance :: [Int] -> Maybe (Int, Int)
calculateBestDistance crabs =
  foldl f Nothing [0 .. length crabs]
  where
    f :: Maybe (Int, Int) -> Int -> Maybe (Int, Int)
    f best offset =
      let currentTotal = sum $ fmap (\x -> abs (x - offset)) crabs
       in case best of
            Nothing -> Just (offset, currentTotal)
            previous@(Just (_, previousTotal)) ->
              if previousTotal < currentTotal
                then previous
                else Just (offset, currentTotal)

sumToN :: Int -> Int
sumToN x =
  if half * 2 == x
    then (x + 1) * half
    else (x * half) + x
  where
    half = x `div` 2

calculateBestDistance2 :: [Int] -> Maybe (Int, Int)
calculateBestDistance2 crabs =
  foldl f Nothing [0 .. length crabs]
  where
    f :: Maybe (Int, Int) -> Int -> Maybe (Int, Int)
    f best offset =
      let currentTotal = sum $ fmap (\x -> sumToN (abs (x - offset))) crabs
       in case best of
            Nothing -> Just (offset, currentTotal)
            previous@(Just (_, previousTotal)) ->
              if previousTotal < currentTotal
                then previous
                else Just (offset, currentTotal)

solution1 :: IO (Maybe (Int, Int))
solution1 =
  calculateBestDistance <$> readSequence

solution2 :: IO (Maybe (Int, Int))
solution2 = do
  calculateBestDistance2 <$> readSequence
