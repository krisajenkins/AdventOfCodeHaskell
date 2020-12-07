{-# LANGUAGE ScopedTypeVariables #-}

module Year2020.Day7
  ( solution1
  , solution2
  ) where

import           Control.Monad        (void)
import           Data.Functor         (($>))
import           Data.Map             (Map)
import qualified Data.Map             as Map
import           Data.Maybe
import           Data.Set             (Set)
import qualified Data.Set             as Set
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Utils                (Parser, integer, simpleParse)

type Rule = (String, [(Int, String)])

pBag :: Parser m String
pBag =
  label "Bag" $
  choice [letterChar, spaceChar] `manyTill`
  choice [string " bags", string " bag"]

pChild :: Parser m (Int, String)
pChild =
  label "Child" $ do
    n <- integer
    void space
    c <- pBag
    pure (n, c)

pRule :: Parser m Rule
pRule =
  label "Rule" $ do
    color <- pBag
    void space
    void $ string "contain"
    void space
    children <-
      choice
        [string "no other bags" $> mempty, pChild `sepBy` (char ',' *> space)]
    void $ char '.'
    pure (color, children)

readSequence :: IO [Rule]
readSequence = simpleParse "data/Year2020/Day7.txt" (pRule `sepEndBy` eol)

buildChildFirstGraph :: [Rule] -> Map String [String]
buildChildFirstGraph =
  Map.fromListWith (<>) .
  foldMap
    (\(parent, children) ->
       foldMap (\(_, child) -> [(child, [parent])]) children)

start :: String
start = "shiny gold"

solution1 :: IO Int
solution1 = do
  rules <- readSequence
  let graph = buildChildFirstGraph rules
  let walk :: String -> Set String
      walk parent =
        let children :: [String]
            children = fromMaybe mempty $ Map.lookup parent graph
         in Set.singleton parent <> Set.unions (walk <$> children)
  pure $ Set.size $ Set.delete start $ walk start

buildParentFirstGraph :: [Rule] -> Map String [(Int, String)]
buildParentFirstGraph = Map.fromList

solution2 :: IO Int
solution2 = do
  rules <- readSequence
  let graph = buildParentFirstGraph rules
  let walk :: (Int, String) -> Int
      walk (n, parent) =
        let children :: [(Int, String)]
            children = fromMaybe mempty $ Map.lookup parent graph
         in n + (n * sum (walk <$> children))
  pure $ walk (1, start) - 1
