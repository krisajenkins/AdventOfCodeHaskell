{-# LANGUAGE RecordWildCards #-}

module Year2018.Day3
  ( solution1
  , solution2
  , PatchId(..)
  ) where

import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Void (Void)
import Text.Megaparsec (ParsecT, sepEndBy)
import Text.Megaparsec.Char (char, eol, space)
import Utils (integer, simpleParse)

newtype PatchId =
  PatchId Int
  deriving (Show, Eq, Ord)

data Patch =
  Patch
    { patchId :: !PatchId
    , x :: !Int
    , y :: !Int
    , w :: !Int
    , h :: !Int
    }
  deriving (Show, Eq, Ord)

parser :: ParsecT Void String IO Patch
parser = do
  _ <- char '#'
  patchId <- PatchId <$> integer
  _ <- space
  _ <- char '@'
  _ <- space
  x <- integer
  _ <- char ','
  y <- integer
  _ <- char ':'
  _ <- space
  w <- integer
  _ <- char 'x'
  h <- integer
  pure Patch {..}

applyPatch ::
     Map (Int, Int) (Set PatchId) -> Patch -> Map (Int, Int) (Set PatchId)
applyPatch existing Patch {..} =
  Map.unionWith (<>) existing $
  Map.fromList $ do
    i <- [x .. x + (w - 1)]
    j <- [y .. y + (h - 1)]
    pure ((i, j), Set.singleton patchId)

readSequence :: IO [Patch]
readSequence = simpleParse "data/Year2018/Day3.txt" (parser `sepEndBy` eol)

-- | What a great name. It's the cloth with the pattern of ownership woven in. :-D
weaveRug :: [Patch] -> Map (Int, Int) (Set PatchId)
weaveRug = foldl applyPatch Map.empty

extractOverlaps :: [Set a] -> [Set a]
extractOverlaps = filter (\pIds -> Set.size pIds > 1)

solution1 :: IO Int
solution1 = length . extractOverlaps . Map.elems . weaveRug <$> readSequence

solution2 :: IO (Set PatchId)
solution2 = do
  rug <- weaveRug <$> readSequence
  let allIds :: Set PatchId
      allIds = Set.unions $ Map.elems rug
      overlappingIds :: Set PatchId
      overlappingIds = Set.unions $ extractOverlaps $ Map.elems rug
  pure $ Set.difference allIds overlappingIds
