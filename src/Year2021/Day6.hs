{-# LANGUAGE ScopedTypeVariables #-}

module Year2021.Day6
  ( solution1,
    solution2,
    populationCount,
    totalPopulation,
    totalPopulation2,
    Day (..),
    SpawnAfter (..),
    readSequence,
  )
where

import Control.Monad.State
import Data.Function ((&))
import Data.Sequence (Seq (Empty, (:<|)))
import qualified Data.Sequence as Seq
import Text.Megaparsec (sepEndBy)
import Text.Megaparsec.Char (char)
import Utils (Parser, applyN, integer, simpleParse)

newtype SpawnAfter = SpawnAfter Int
  deriving (Show, Eq, Ord)

newtype Day = Day Int
  deriving (Show, Eq, Ord)

parseSpawnAfter :: Parser m SpawnAfter
parseSpawnAfter = SpawnAfter <$> integer

readSequence :: IO [SpawnAfter]
readSequence = simpleParse "data/Year2021/Day6.txt" (parseSpawnAfter `sepEndBy` char ',')

populationCount :: (Day -> SpawnAfter -> Integer) -> Day -> SpawnAfter -> Integer
populationCount f (Day day) (SpawnAfter spawn)
  | day <= 0 = 1
  | spawn == 0 = f tomorrow (SpawnAfter 6) + f tomorrow (SpawnAfter 8)
  | otherwise = f (Day (day - spawn)) (SpawnAfter 0)
  where
    tomorrow = Day (day - 1)

totalPopulation :: Day -> [SpawnAfter] -> Integer
totalPopulation day fish =
  sum $ fix populationCount day <$> fish

totalPopulation2 :: Day -> [SpawnAfter] -> Integer
totalPopulation2 (Day day) fish =
  sum $ applyN day step initialSet
  where
    initialSet = foldl (increment 1) (Seq.replicate (day + 10) 0) fish

    increment :: Integer -> Seq Integer -> SpawnAfter -> Seq Integer
    increment n set (SpawnAfter spawnAfter) = Seq.adjust (+ n) spawnAfter set

    step :: Seq Integer -> Seq Integer
    step Empty = Empty
    step (x :<| xs) =
      xs
        & Seq.adjust (+ x) 6
        & Seq.adjust (+ x) 8

solution1 :: IO Integer
solution1 = totalPopulation2 (Day 80) <$> readSequence

solution2 :: IO Integer
solution2 = totalPopulation2 (Day 256) <$> readSequence
