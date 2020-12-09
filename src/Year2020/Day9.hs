{-# LANGUAGE ScopedTypeVariables #-}

module Year2020.Day9
  ( solution1
  , solution2
  ) where

import           Data.Maybe
import           Data.Set             (Set)
import qualified Data.Set             as Set
import           Safe                 (headMay, maximumMay, minimumMay)
import           Text.Megaparsec      (sepEndBy)
import           Text.Megaparsec.Char (eol)
import           Utils                (integer, simpleParse)

readSequence :: IO [Int]
readSequence = simpleParse "data/Year2020/Day9.txt" (integer `sepEndBy` eol)

choose2 :: Ord a => Set a -> [(a, a)]
choose2 xs = do
  x <- Set.toList xs
  let ys = Set.delete x xs
  y <- Set.toList ys
  pure (x, y)

partition :: [a] -> [([a], a)]
partition xs =
  let (start, end) = (take 25 xs, drop 25 xs)
   in case headMay end of
        Nothing -> []
        Just y  -> (start, y) : partition end

isValid :: (Ord a, Num a) => ([a], a) -> Bool
isValid (xs, x) = x `elem` fmap (uncurry (+)) (choose2 (Set.fromList xs))

solution1 :: IO (Maybe Int)
solution1 = do
  xs <- readSequence
  let questions = partition xs
  let answer = headMay $ filter (not . isValid) questions
  pure (snd <$> answer)

generateSubsets :: [a] -> [[a]]
generateSubsets []        = []
generateSubsets xs@(_:ys) = xs : generateSubsets ys

findSubsectionSummingTo :: Int -> [Int] -> Maybe [Int]
findSubsectionSummingTo target = go []
  where
    go acc [] =
      if sum acc == target
        then Just acc
        else Nothing
    go acc (x:xs) =
      let newAcc = x : acc
       in case compare (sum newAcc) target of
            EQ -> Just newAcc
            LT -> go newAcc xs
            GT -> Nothing

solution2 :: IO (Maybe Int)
solution2 = do
  xs <- readSequence
  mTarget <- solution1
  let result = do
        target <- mTarget
        let subsets :: [[Int]]
            subsets = generateSubsets xs
        matching <- headMay $ catMaybes $ findSubsectionSummingTo target <$> subsets
        (+) <$> minimumMay matching <*> maximumMay matching
  pure result
