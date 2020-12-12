{-# LANGUAGE ScopedTypeVariables #-}

module Year2020.Day11
  ( solution1
  , solution2
  ) where

import           Control.Monad        (guard)
import           Data.Map.Strict      (Map)
import qualified Data.Map.Strict      as Map
import           Data.Maybe           (mapMaybe)
import           Text.Megaparsec      (choice, sepEndBy, some)
import           Text.Megaparsec.Char (char, eol)
import           Utils                (simpleParse)

readSequence :: IO [String]
readSequence =
  simpleParse
    "data/Year2020/Day11.txt"
    (some (choice [char '.', char '#', char 'L']) `sepEndBy` eol)

toMap :: [String] -> Map (Int, Int) Char
toMap xs =
  Map.fromList $ do
    (r, rows) <- zip [0 ..] xs
    (c, item) <- zip [0 ..] rows
    guard (item /= '.')
    pure ((c, r), item)

step1 :: Grid Char -> Grid Char
step1 grid = mapWithKey adjacencyRule grid
  where
    adjacencyRule (x, y) 'L'
      | countAdjacent (x, y) == 0 = '#'
    adjacencyRule (x, y) '#'
      | countAdjacent (x, y) >= 4 = 'L'
    adjacencyRule _ c = c
    countAdjacent (x, y) =
      length $
      filter (== '#') $
      mapMaybe
        (lookupOffset grid (x, y))
        [(-1, -1), (0, -1), (1, -1), (-1, 0), (1, 0), (-1, 1), (0, 1), (1, 1)]

step2 :: Grid Char -> Grid Char
step2 grid = mapWithKey adjacencyRule grid
  where
    adjacencyRule (x, y) 'L'
      | countAdjacent (x, y) == 0 = '#'
    adjacencyRule (x, y) '#'
      | countAdjacent (x, y) >= 5 = 'L'
    adjacencyRule _ c = c
    countAdjacent (x, y) =
      length $
      filter (== '#') $
      mapMaybe
        (searchOffset grid (x, y))
        [(-1, -1), (0, -1), (1, -1), (-1, 0), (1, 0), (-1, 1), (0, 1), (1, 1)]

data Grid v =
  Grid (Int, Int) (Map (Int, Int) v)
  deriving (Show, Eq)

mapWithKey :: ((Int, Int) -> v1 -> v2) -> Grid v1 -> Grid v2
mapWithKey f (Grid bounds m) = Grid bounds (Map.mapWithKey f m)

lookupOffset :: Grid v -> (Int, Int) -> (Int, Int) -> Maybe v
lookupOffset (Grid (width, height) m) (x, y) (offsetX, offsetY) = do
  let (px, py) = (x + offsetX, y + offsetY)
  guard (0 <= px && px < width)
  guard (0 <= py && py < height)
  Map.lookup (px, py) m

searchOffset :: Grid v -> (Int, Int) -> (Int, Int) -> Maybe v
searchOffset grid@(Grid (width, height) m) (x, y) offset@(offsetX, offsetY) = do
  let (px, py) = (x + offsetX, y + offsetY)
  guard (0 <= px && px < width)
  guard (0 <= py && py < height)
  guard ((px, py) /= (x, y))
  case Map.lookup (px, py) m of
    Nothing -> searchOffset grid (px, py) offset
    Just v  -> pure v

stepUntilStable :: Eq a => (a -> a) -> a -> a
stepUntilStable stepFn m =
  let next = stepFn m
   in if next == m
        then next
        else stepUntilStable stepFn next

countOccupied :: Map k Char -> Int
countOccupied = length . filter (== '#') . Map.elems

countOccupiedGrid :: Grid Char -> Int
countOccupiedGrid (Grid _ m) = countOccupied m

solution1 :: IO Int
solution1 = countOccupiedGrid . stepUntilStable step1 . toGrid <$> readSequence

toGrid :: [String] -> Grid Char
toGrid xs = Grid (maximum (length <$> xs), length xs) (toMap xs)

solution2 :: IO Int
solution2 = do
  g <- toGrid <$> readSequence
  pure $ countOccupiedGrid $ stepUntilStable step2 g
