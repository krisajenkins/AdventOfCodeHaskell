{-# LANGUAGE ScopedTypeVariables #-}

module Year2018.Day1
  ( solution1
  , solution2
  ) where

import Data.Monoid (Sum(Sum), getSum)
import Data.Set (insert, member)
import Text.Megaparsec (sepEndBy)
import Text.Megaparsec.Char (eol)
import Utils (integer, simpleParse)

------------------------------------------------------------
readSequence :: IO [Int]
readSequence = simpleParse "data/Year2018/Day1.txt" (integer `sepEndBy` eol)

solution1 :: IO Int
solution1 = getSum . foldMap Sum <$> readSequence

ignoreNewbies :: Ord a => [a] -> [a]
ignoreNewbies = go mempty
  where
    go _ [] = []
    go seen (x:xs) =
      if x `member` seen
        then x : rest
        else rest
      where
        rest = go (x `insert` seen) xs

solution2 :: IO Int
solution2 = head . ignoreNewbies . scanl (+) 0 . cycle <$> readSequence
