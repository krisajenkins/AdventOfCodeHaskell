{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Year2021.Day5
  ( solution1,
    solution2,
  )
where

import Control.Lens (view)
import Control.Lens.TH (makeLenses)
import Control.Monad (void)
import Data.Map (Map)
import qualified Data.Map as Map
import Text.Megaparsec (sepEndBy)
import Text.Megaparsec.Char (char, eol, string)
import Utils (Parser, frequency, integer, simpleParse)

data Point = Point
  { _x :: Int,
    _y :: Int
  }
  deriving (Show, Eq, Ord)

makeLenses ''Point

data Line = Line
  { _from :: Point,
    _to :: Point
  }
  deriving (Show, Eq)

makeLenses ''Line

parsePoint :: Parser m Point
parsePoint = do
  _x <- integer
  void $ char ','
  _y <- integer
  pure $ Point {_x, _y}

parseLine :: Parser m Line
parseLine = do
  _from <- parsePoint
  void $ string " -> "
  _to <- parsePoint
  pure $ Line {_from, _to}

readSequence :: IO [Line]
readSequence = simpleParse "data/Year2021/Day5.txt" (parseLine `sepEndBy` eol)

overlapCount :: [Line] -> Int
overlapCount ventLines =
  let hits :: [Point]
      hits = concatMap lineToPoints ventLines
      hitMap :: Map Point Int
      hitMap = frequency hits
   in Map.size $ Map.filter (> 1) hitMap

lineToPoints :: Line -> [Point]
lineToPoints line
  | x1 == x2 = (\_y -> Point {_x = x1, _y}) <$> [min y1 y2 .. max y1 y2]
  | y1 == y2 = (\_x -> Point {_x, _y = y1}) <$> [min x1 x2 .. max x1 x2]
  | abs (y2 - y1) == abs (x2 - x1) =
    let delta = abs (x2 - x1)
     in ( \n ->
            let _x = if x1 < x2 then x1 + n else x1 - n
                _y = if y1 < y2 then y1 + n else y1 - n
             in Point {_x, _y}
        )
          <$> [0 .. delta]
  | otherwise =
    error "not implemented"
  where
    x1 = view (from . x) line
    y1 = view (from . y) line
    x2 = view (to . x) line
    y2 = view (to . y) line

isHorizontalOrVertical :: Line -> Bool
isHorizontalOrVertical line =
  (view (from . x) line == view (to . x) line)
    || (view (from . y) line == view (to . y) line)

solution1 :: IO Int
solution1 =
  overlapCount . filter isHorizontalOrVertical <$> readSequence

solution2 :: IO Int
solution2 =
  overlapCount <$> readSequence
