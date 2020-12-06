{-# LANGUAGE ScopedTypeVariables #-}

module Year2020.Day5 where

import           Data.Bits            (setBit, shift, zeroBits)
import           Data.List            (sort)
import qualified Data.Set             as Set
import           Safe                 (headMay)
import           Text.Megaparsec      (sepEndBy, some)
import           Text.Megaparsec.Char (eol, letterChar)
import           Utils                (simpleParse)

decodeBoardingPass :: String -> Int
decodeBoardingPass = foldl (f . flip shift 1) zeroBits
  where
    f :: Int -> Char -> Int
    f n 'B' = n `setBit` 0
    f n 'R' = n `setBit` 0
    f n _   = n

readSequence :: IO [String]
readSequence =
  simpleParse "data/Year2020/Day5.txt" (some letterChar `sepEndBy` eol)

solution1 :: IO Int
solution1 = maximum . fmap decodeBoardingPass <$> readSequence

solution2 :: IO (Maybe Int)
solution2 = do
  seatIds <- Set.fromList . sort . fmap decodeBoardingPass <$> readSequence
  pure $ headMay $ filter (`Set.notMember` seatIds) [Set.findMin seatIds ..]
