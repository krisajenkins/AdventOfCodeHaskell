{-# LANGUAGE ScopedTypeVariables #-}

module Year2020.Day8 where

import Control.Monad (void)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector, (!?))
import qualified Data.Vector as Vector
import Safe
import Text.Megaparsec
import Text.Megaparsec.Char
import Utils (Parser, integer, simpleParse)

data Instruction
  = Acc Int
  | NoOp Int
  | Jump Int
  deriving (Show, Eq)

data Termination
  = HaltedWith Int
  | LoopedWith Int
  deriving (Show, Eq)

number :: Parser m Int
number = optional (char '+') *> integer

pInstruction :: Parser m Instruction
pInstruction =
  choice
    [ do void $ string "acc"
         space
         value <- number
         pure $ Acc value
    , do void $ string "nop"
         space
         value <- number
         pure $ NoOp value
    , do void $ string "jmp"
         space
         value <- number
         pure $ Jump value
    ]

readSequence :: IO [Instruction]
readSequence =
  simpleParse "data/Year2020/Day8.txt" (pInstruction `sepEndBy` eol)

evaluateOnce :: Vector Instruction -> Termination
evaluateOnce instructions = go 0 0 Set.empty
  where
    go :: Int -> Int -> Set Int -> Termination
    go step accumulator seenSteps =
      if (Set.member step seenSteps)
        then LoopedWith accumulator
        else case instructions !? step of
               Nothing -> HaltedWith accumulator
               Just nextInstruction ->
                 let fStep :: Int -> Int
                     fAccumulator :: Int -> Int
                     (fStep, fAccumulator) =
                       case nextInstruction of
                         NoOp _ -> (\x -> x + 1, \x -> x)
                         Acc n -> (\x -> x + 1, \x -> x + n)
                         Jump n -> (\x -> x + n, \x -> x)
                  in go
                       (fStep step)
                       (fAccumulator accumulator)
                       (Set.insert step seenSteps)

solution1 :: IO Termination
solution1 = do
  instructions <- Vector.fromList <$> readSequence
  pure $ evaluateOnce instructions

mutateProgram :: Vector Instruction -> [Vector Instruction]
mutateProgram instructions =
  Vector.toList $ do
    n <- Vector.findIndices isMutatable instructions
    pure $ Vector.accum (\i _ -> mutate i) instructions [(n, ())]
  where
    isMutatable x = x /= mutate x
    mutate (Acc n) = Acc n
    mutate (Jump n) = NoOp n
    mutate (NoOp n) = Jump n

findHaltingProgram :: [Vector Instruction] -> Maybe Termination
findHaltingProgram = headMay . filter didHalt . fmap evaluateOnce
  where
    didHalt (HaltedWith _) = True
    didHalt (LoopedWith _) = False

solution2 :: IO (Maybe Termination)
solution2 = do
  instructions <- Vector.fromList <$> readSequence
  pure $ findHaltingProgram $ mutateProgram instructions
