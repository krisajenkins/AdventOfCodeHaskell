{-# LANGUAGE ScopedTypeVariables #-}

module Year2020.Day5 where

import           Data.Bits            (setBit, shift, zeroBits)
import           Data.List            (sort)
import           Data.List.Extra      (maximumOn)
import qualified Data.Set             as Set
import           Safe                 (headMay)
import           Text.Megaparsec      (sepEndBy, some)
import           Text.Megaparsec.Char (eol, letterChar)
import           Utils                (simpleParse)

data Seat =
  Seat
    { row    :: Int
    , column :: Int
    , seatId :: Int
    }
  deriving (Show, Eq)

decodeBoardingPass :: String -> Seat
decodeBoardingPass =
  (\(r, c) -> Seat {row = r, column = c, seatId = (r * 8) + c}) .
  foldl f (zeroBits, zeroBits)
  where
    f :: (Int, Int) -> Char -> (Int, Int)
    f (r, c) 'F' = (shift r 1, c)
    f (r, c) 'B' = (shift r 1 `setBit` 0, c)
    f (r, c) 'L' = (r, shift c 1)
    f (r, c) 'R' = (r, shift c 1 `setBit` 0)
    f (r, c) _   = (r, c)

readSequence :: IO [String]
readSequence =
  simpleParse "data/Year2020/Day5.txt" (some letterChar `sepEndBy` eol)

solution1 :: IO Seat
solution1 = maximumOn seatId . fmap decodeBoardingPass <$> readSequence

solution2 :: IO (Maybe Int)
solution2 = do
  seatIds <-
    Set.fromList . sort . fmap (seatId . decodeBoardingPass) <$> readSequence
  pure $ headMay $ filter (`Set.notMember` seatIds) [Set.findMin seatIds ..]
