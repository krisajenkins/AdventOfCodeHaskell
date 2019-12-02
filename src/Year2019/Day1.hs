{-# LANGUAGE ScopedTypeVariables #-}

module Year2019.Day1
  ( solution1
  , totalFullFuelRequirement
  , solution2
  ) where

import           Data.Char            (isLower, toUpper)
import           Data.Foldable        (traverse_)
import           Data.Function        (fix)
import           Data.Functor         (($>))
import           Data.List            (sort, sortOn, unfoldr)
import           Data.Time            (UTCTime, defaultTimeLocale, parseTimeM)
import           Data.Void            (Void)
import           Text.Megaparsec      (ParsecT, between, many, manyTill,
                                       sepEndBy, (<|>))
import           Text.Megaparsec.Char (anyChar, char, eol, notChar, space,
                                       string)
import           Utils                (integer, simpleParse)

readSequence :: IO [Int]
readSequence = simpleParse "data/Year2019/Day1.txt" (integer `sepEndBy` eol)

simpleFuelRequirement :: Int -> Int
simpleFuelRequirement n = max 0 ((n `div` 3) - 2)

totalSimpleFuelRequirement :: [Int] -> Int
totalSimpleFuelRequirement = sum . fmap simpleFuelRequirement

fullFuelRequirement :: Int -> Int
fullFuelRequirement = sum . unfoldr f
  where
    f :: Int -> Maybe (Int, Int)
    f 0 = Nothing
    f x = Just (simpleFuelRequirement x, simpleFuelRequirement x)

totalFullFuelRequirement :: [Int] -> Int
totalFullFuelRequirement = sum . fmap fullFuelRequirement

solution1 :: IO Int
solution1 = totalSimpleFuelRequirement <$> readSequence

solution2 :: IO Int
solution2 = totalFullFuelRequirement <$> readSequence
