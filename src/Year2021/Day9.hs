{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Year2021.Day9
  ( solution1,
    solution2,
    isLowPoint,
  )
where

import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Ord (Down (Down))
import Data.Set (Set)
import qualified Data.Set as Set
import Utils (Point (..), readGrid, singleDigit)

readSequence :: IO (Map Point Integer)
readSequence = readGrid singleDigit "data/Year2021/Day9.txt"

isLowPoint :: Map Point Integer -> Point -> Integer -> Bool
isLowPoint m Point {_x, _y} value =
  value < up
    && value < down
    && value < left
    && value < right
  where
    check dx dy = fromMaybe 10 $ Map.lookup (Point (_x + dx) (_y + dy)) m
    up = check 0 (-1)
    down = check 0 1
    left = check (-1) 0
    right = check 1 0

solution1 :: IO Integer
solution1 = do
  grid <- readSequence
  let lowPoints = Map.filterWithKey (isLowPoint grid) grid
  let risks = (+ 1) <$> Map.elems lowPoints
  pure $ sum risks

findBasin :: Map Point Integer -> Point -> Set Point
findBasin grid start = go [start] Set.empty
  where
    go :: [Point] -> Set Point -> Set Point
    go toVisit inBasin =
      case toVisit of
        [] -> inBasin
        (nextPoint@Point {_x, _y} : rest) ->
          let check :: Int -> Int -> Maybe Point
              check dx dy =
                let newPoint :: Point
                    newPoint = Point (_x + dx) (_y + dy)
                 in newPoint <$ Map.lookup newPoint grid
              up = check 0 (-1)
              down = check 0 1
              left = check (-1) 0
              right = check 1 0
              expand :: [Point]
              expand = filter (`Set.notMember` inBasin) $ catMaybes [up, down, left, right]
           in case Map.lookup nextPoint grid of
                Just value
                  | value < 9 ->
                    go
                      (rest <> expand)
                      (Set.insert nextPoint inBasin)
                _ -> go (tail toVisit) inBasin

solution2 :: IO Int
solution2 = do
  grid <- readSequence
  let lowPoints :: Map Point Integer
      lowPoints = Map.filterWithKey (isLowPoint grid) grid
      basins :: [Set Point]
      basins = findBasin grid <$> Map.keys lowPoints
  pure $ product $ take 3 $ sortOn Down $ Set.size <$> basins
