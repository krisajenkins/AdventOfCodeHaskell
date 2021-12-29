{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Year2021.Day11
  ( solution1,
    solution2,
    step,
    Octopus (..),
  )
where

import Control.Lens (over)
import Control.Monad (guard)
import Data.Bifunctor (Bifunctor (second))
import Data.Map (Map)
import qualified Data.Map as Map
import Utils (Point, applyN, readGrid, singleDigit, x, y)
import Prelude hiding (lookup)

data Octopus
  = Value Integer
  | Flashed
  deriving (Show, Eq, Ord)

type Grid = Map Point Octopus

readSequence :: IO Grid
readSequence = readGrid (Value <$> singleDigit) "data/Year2021/Day11.txt"

increment :: Octopus -> Octopus
increment (Value n) = Value (n + 1)
increment Flashed = Flashed

reset :: Octopus -> Octopus
reset Flashed = Value 0
reset (Value n) = Value n

isReadyToFlash :: Octopus -> Bool
isReadyToFlash = \case
  (Value n)
    | n > 9 ->
      True
  _ -> False

step :: (Integer, Grid) -> (Integer, Grid)
step (total, grid) =
  second (Map.map reset) $ flashNeighbours (total, Map.map increment grid)

surround :: [(Int, Int)]
surround =
  do
    dx <- [-1, 0, 1]
    dy <- [-1, 0, 1]
    guard (not (dx == 0 && dy == 0))
    pure (dx, dy)

flashNeighbours :: (Integer, Grid) -> (Integer, Grid)
flashNeighbours (oldTotal, oldGrid) =
  let keysToFlash = Map.keys $ Map.filter isReadyToFlash oldGrid
   in if null keysToFlash
        then (oldTotal, oldGrid)
        else
          flashNeighbours
            ( oldTotal + toInteger (length keysToFlash),
              foldl
                ( \newGrid point ->
                    let incrementSurround ng =
                          foldr
                            (Map.update (Just . increment))
                            ng
                            ( fmap
                                ( \(dx, dy) ->
                                    over x (+ dx) (over y (+ dy) point)
                                )
                                surround
                            )
                     in incrementSurround $ Map.insert point Flashed newGrid
                )
                oldGrid
                keysToFlash
            )

solution1 :: IO Integer
solution1 = do
  grid <- readSequence
  pure $ fst $ applyN 100 step (0, grid)

solution2 :: IO Int
solution2 =
  countStepsUntilAllFlash 1 <$> readSequence

countStepsUntilAllFlash :: Int -> Grid -> Int
countStepsUntilAllFlash count grid =
  let (flashes, newGrid) = step (0, grid)
   in if flashes == toInteger (Map.size grid)
        then count
        else countStepsUntilAllFlash (count + 1) newGrid
