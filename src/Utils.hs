{-# LANGUAGE TemplateHaskell #-}

module Utils where

import Control.Applicative (Alternative ((<|>)), some)
import Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import Data.Bifunctor
import Data.Functor (($>), (<&>))
import qualified Data.List as List
import Data.Map (Map, unionsWith)
import qualified Data.Map as Map
import Data.Monoid (Sum (Sum), getSum)
import Data.Void (Void)
import Debug.Trace (trace)
import Paths_adventofcode (getDataFileName)
import Text.Megaparsec
  ( ParseErrorBundle,
    ParsecT,
    errorBundlePretty,
    runParserT,
    sepEndBy,
  )
import Text.Megaparsec.Char (char, eol)
import Text.Megaparsec.Char.Lexer (decimal, lexeme, signed)
import Control.Lens (makeLenses)

type Parser = ParsecT Void String

type ParseErrors = ParseErrorBundle String Void

_simpleParse ::
  FilePath ->
  Parser IO a ->
  IO (Either ParseErrors a)
_simpleParse datafile parser = do
  filename <- getDataFileName datafile
  contents <- readFile filename
  runParserT parser filename contents

simpleParse :: FilePath -> Parser IO a -> IO a
simpleParse datafile parser =
  _simpleParse datafile parser
    >>= failOnError

failOnError :: Either ParseErrors a -> IO a
failOnError (Left err) = fail $ errorBundlePretty err
failOnError (Right value) = pure value

simpleParseWithError :: FilePath -> Parser IO a -> IO (Either String a)
simpleParseWithError datafile parser =
  _simpleParse datafile parser <&> first errorBundlePretty

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

------------------------------------------------------------
class Functor f => FunctorWithIndex f where
  mapWithIndex :: (Int -> a -> b) -> f a -> f b

instance FunctorWithIndex [] where
  mapWithIndex f xs = uncurry f <$> zip [0 ..] xs

------------------------------------------------------------
class Foldable f => FoldableWithIndex f where
  foldlWithIndex :: (Int -> b -> a -> b) -> b -> f a -> b

instance FoldableWithIndex [] where
  foldlWithIndex f acc xs = foldl (\b (index, a) -> f index b a) acc $ zip [0 ..] xs

------------------------------------------------------------
applyN :: Int -> (a -> a) -> a -> a
applyN 0 _ x = x
applyN n f x = applyN (n -1) f (f x)

traceMsg :: Show a => String -> a -> a
traceMsg msg x = trace (msg <> ": " <> show x) x

flipMap :: Ord v => Map k v -> Map v k
flipMap =
  Map.fromList
    . fmap (\(a, b) -> (b, a))
    . Map.toList

-- | The magic spreadsheet maker! Give it some functions that can
-- refer to the final result. As long as there are no cycles, it will
-- figure out the correct function resolution order for you!
--
-- Tip: Pick a Functor (like list or map) and make the type signature concrete. For example:
--
-- loebMap :: Map Int (Map Int a -> a) -> Map Int a
-- You can build a map of ints to values by supplying a map of ints to
-- "functions that can look at the final answer to generate their
-- result."
--
-- See https://github.com/quchen/articles/blob/master/loeb-moeb.md for details
loeb :: Functor f => f (f a -> a) -> f a
loeb x = go where go = fmap ($ go) x

singleDigit :: Parser m Integer
singleDigit =
  (char '0' $> 0)
    <|> (char '1' $> 1)
    <|> (char '2' $> 2)
    <|> (char '3' $> 3)
    <|> (char '4' $> 4)
    <|> (char '5' $> 5)
    <|> (char '6' $> 6)
    <|> (char '7' $> 7)
    <|> (char '8' $> 8)
    <|> (char '9' $> 9)

data Point = Point {_x :: Int, _y :: Int}
  deriving (Show, Eq, Ord)

makeLenses ''Point

toGrid :: [[a]] -> Map Point a
toGrid points = Map.fromList $ concat $ mapWithIndex (\py row -> mapWithIndex (\px value -> (Point px py, value)) row) points

readGrid :: Parser IO a -> FilePath -> IO (Map Point a)
readGrid p filePath = do
  toGrid <$> simpleParse filePath (some p `sepEndBy` eol)
