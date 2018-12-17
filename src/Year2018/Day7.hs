{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Year2018.Day7 where

import Control.Lens (_1, _2, assign, modifying, use, zoom)
import Control.Lens.TH (makeLenses)
import Control.Monad (void)
import Control.Monad.Free (Free(Pure))
import Control.Monad.Trans.State (State, evalState, runState)
import Data.Char (ord)
import Data.Functor.Foldable (ListF(Cons, Nil), futu)
import Data.List (partition, sortBy)
import qualified Data.Map as Map
import Data.Map (Map, keysSet)
import Data.Maybe (isNothing)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Void (Void)
import GraphUtils (toGraph)
import Safe (headMay)
import Safe.Foldable (minimumMay)
import Text.Megaparsec (ParsecT, sepEndBy)
import Text.Megaparsec.Char (anyChar, eol, string)
import Utils (simpleParse)

edgeParser :: ParsecT Void String IO (Char, Char)
edgeParser = do
  void $ string "Step "
  from <- anyChar
  void $ string " must be finished before step "
  to <- anyChar
  void $ string " can begin."
  pure (from, to)

------------------------------------------------------------
readSequence :: IO [(Char, Char)]
readSequence = simpleParse "data/Year2018/Day7.txt" (edgeParser `sepEndBy` eol)

elemsSet :: Ord v => Map k (Set v) -> Set v
elemsSet = Set.unions . Map.elems

assemble0 :: Ord a => Map a (Set a) -> [a]
assemble0 graph =
  let satisfiable = Set.difference (keysSet graph) (elemsSet graph)
   in case minimumMay satisfiable of
        Nothing -> []
        Just next -> next : assemble0 (Map.delete next graph)

assemble :: Map Char (Set Char) -> String
assemble =
  futu $ \graph ->
    let satisfiable = Set.difference (keysSet graph) (elemsSet graph)
     in case minimumMay satisfiable of
          Nothing -> Nil
          Just next -> Cons next (Pure (Map.delete next graph))

solution1 :: IO String
solution1 = assemble . toGraph <$> readSequence

------------------------------------------------------------
costOf :: Char -> Int
costOf c = ord c - 4

type Worker = (Int, Maybe Char)

type Queue = Map Char Int

type Graph = Map Char (Set Char)

type Visited = Set Char

data Instruction
  = AddToQueue [(Char, Int)]
  | ProcessQueue
  | Completed Int
              Char
  | Done
  deriving (Show, Eq)

data SchedulerState = SchedulerState
  { _schedulerGraph :: Graph
  , _schedulerVisited :: Visited
  } deriving (Show, Eq)

data WorkerState = WorkerState
  { _workerQueue :: Queue
  , _workerWorkers :: [Worker]
  } deriving (Show, Eq)

makeLenses ''SchedulerState

makeLenses ''WorkerState

interpreter :: Graph -> [Worker]
interpreter graph =
  let (firstInstruction, firstState) =
        runState
          (zoom _1 (scheduler 0 Nothing))
          ( SchedulerState graph Set.empty
          , WorkerState Map.empty (replicate 5 (0, Nothing)))
   in evalState (simulate firstInstruction) firstState
  where
    simulate :: Instruction -> State (SchedulerState, WorkerState) [Worker]
    simulate instruction =
      case instruction of
        Completed time char -> do
          next <- zoom _1 $ scheduler time (Just char)
          simulate next
        Done -> use (_2 . workerWorkers)
        AddToQueue newEntries -> do
          modifying (_2 . workerQueue) $
            Map.unionWith min (Map.fromList newEntries)
          simulate ProcessQueue
        ProcessQueue -> do
          next <- zoom _2 work
          simulate next

scheduler :: Int -> Maybe Char -> State SchedulerState Instruction
scheduler time (Just char) = do
  modifying schedulerGraph (Map.delete char)
  scheduler time Nothing
scheduler time Nothing = do
  graph <- use schedulerGraph
  visited <- use schedulerVisited
  let newNodes =
        Set.difference (Set.difference (keysSet graph) (elemsSet graph)) visited
      newTasks = (, time) <$> Set.toList newNodes
      newVisited = Set.union visited newNodes
  assign schedulerVisited newVisited
  pure $ AddToQueue newTasks

work :: State WorkerState Instruction
work = do
  queue <- use workerQueue
  workers <- use workerWorkers
  case (headMay $ Map.toList queue, sortBy compareWorkers workers)
    -- | No workers.
        of
    (_, []) -> pure Done
    -- | Consuming
    (Nothing, sortedWorkers) ->
      case partition (isNothing . snd) sortedWorkers of
        (idle, (n, Just c):rest) -> do
          assign workerWorkers (idle <> ((n, Nothing) : rest))
          pure $ Completed n c
        _ -> pure Done
    -- | Replacing
    (Just (c, n), (start, current):rest) -> do
      modifying workerQueue $ Map.delete c
      let completedAt = max start n + costOf c
      assign workerWorkers $ (completedAt, Just c) : rest
      pure $
        case current of
          Nothing -> ProcessQueue
          Just r -> Completed start r
  where
    compareWorkers :: Worker -> Worker -> Ordering
    compareWorkers (_, Nothing) (_, Just _) = LT
    compareWorkers (_, Just _) (_, Nothing) = GT
    compareWorkers (x, _) (y, _) = compare x y

solution2 :: IO Int
solution2 = maximum . map fst . interpreter . toGraph <$> readSequence
