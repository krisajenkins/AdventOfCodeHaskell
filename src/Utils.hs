{-# LANGUAGE LambdaCase #-}

module Utils where

import Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import qualified Data.List as List
import Data.Map (Map, unionsWith)
import qualified Data.Map as Map
import Data.Monoid (Sum (Sum), getSum)
import Data.Void (Void)
import Paths_adventofcode (getDataFileName)
import Text.Megaparsec
  ( ParseErrorBundle,
    ParsecT,
    errorBundlePretty,
    runParserT,
  )
import Text.Megaparsec.Char.Lexer (decimal, lexeme, signed)

type Parser = ParsecT Void String

type ParseErrors = ParseErrorBundle String Void

simpleParse :: FilePath -> Parser IO a -> IO a
simpleParse datafile parser = do
  filename <- getDataFileName datafile
  contents <- readFile filename
  runParserT parser filename contents >>= \case
    Left err -> fail $ errorBundlePretty err
    Right value -> pure value

integer :: Parser m Int
integer = signed (pure ()) (lexeme (pure ()) decimal)

frequency :: Ord a => [a] -> Map a Int
frequency = fmap getSum . unionsWith (<>) . fmap (`Map.singleton` Sum 1)

editDistance :: Eq a => [a] -> [a] -> Int
editDistance xs ys = editD 0 0
  where
    xss = List.tails xs
    yss = List.tails ys
    tbl =
      Map.fromList
        [ ((i, j), editD' i j)
          | i <- [0 .. length xss - 1],
            j <- [0 .. length yss - 1]
        ]
    editD i j = tbl Map.! (i, j)
    editD' i j =
      case (xss !! i, yss !! j) of
        ([], bs) -> length bs
        (as, []) -> length as
        (a : _, b : _)
          | a == b -> editD (i + 1) (j + 1)
          | otherwise ->
            1
              + minimum [editD (i + 1) j, editD i (j + 1), editD (i + 1) (j + 1)]

mapError :: MonadError f m => (e -> f) -> ExceptT e m a -> m a
mapError f action = do
  result <- runExceptT action
  case result of
    Left e -> throwError (f e)
    Right v -> pure v
