module Year2021.Day11Spec
  ( spec,
  )
where

import Data.Map (Map)
import qualified Data.Sequence as Seq
import Test.Hspec (Spec, describe, it, shouldBe)
import Utils
import Year2021.Day11

toOctopusGrid :: [[Integer]] -> Map Point Octopus
toOctopusGrid = toGrid . fmap (fmap Value)

spec :: Spec
spec =
  describe "solutions" $ do
    it "Support" $ do
      let grid1 =
            toOctopusGrid
              [ [1, 1, 1, 1, 1],
                [1, 9, 9, 9, 1],
                [1, 9, 1, 9, 1],
                [1, 9, 9, 9, 1],
                [1, 1, 1, 1, 1]
              ]

      applyN 1 step (0, grid1)
        `shouldBe` ( 9,
                     toOctopusGrid
                       [ [3, 4, 5, 4, 3],
                         [4, 0, 0, 0, 4],
                         [5, 0, 0, 0, 5],
                         [4, 0, 0, 0, 4],
                         [3, 4, 5, 4, 3]
                       ]
                   )

      applyN 2 step (0, grid1)
        `shouldBe` ( 9,
                     toOctopusGrid
                       [ [4, 5, 6, 5, 4],
                         [5, 1, 1, 1, 5],
                         [6, 1, 1, 1, 6],
                         [5, 1, 1, 1, 5],
                         [4, 5, 6, 5, 4]
                       ]
                   )
      let grid2 =
            toOctopusGrid
              [ [5, 4, 8, 3, 1, 4, 3, 2, 2, 3],
                [2, 7, 4, 5, 8, 5, 4, 7, 1, 1],
                [5, 2, 6, 4, 5, 5, 6, 1, 7, 3],
                [6, 1, 4, 1, 3, 3, 6, 1, 4, 6],
                [6, 3, 5, 7, 3, 8, 5, 4, 7, 8],
                [4, 1, 6, 7, 5, 2, 4, 6, 4, 5],
                [2, 1, 7, 6, 8, 4, 1, 7, 2, 1],
                [6, 8, 8, 2, 8, 8, 1, 1, 3, 4],
                [4, 8, 4, 6, 8, 4, 8, 5, 5, 4],
                [5, 2, 8, 3, 7, 5, 1, 5, 2, 6]
              ]
      step (0, grid2)
        `shouldBe` ( 0,
                     toOctopusGrid
                       [ [6, 5, 9, 4, 2, 5, 4, 3, 3, 4],
                         [3, 8, 5, 6, 9, 6, 5, 8, 2, 2],
                         [6, 3, 7, 5, 6, 6, 7, 2, 8, 4],
                         [7, 2, 5, 2, 4, 4, 7, 2, 5, 7],
                         [7, 4, 6, 8, 4, 9, 6, 5, 8, 9],
                         [5, 2, 7, 8, 6, 3, 5, 7, 5, 6],
                         [3, 2, 8, 7, 9, 5, 2, 8, 3, 2],
                         [7, 9, 9, 3, 9, 9, 2, 2, 4, 5],
                         [5, 9, 5, 7, 9, 5, 9, 6, 6, 5],
                         [6, 3, 9, 4, 8, 6, 2, 6, 3, 7]
                       ]
                   )
      applyN 3 step (0, grid2)
        `shouldBe` ( 80,
                     toOctopusGrid
                       [ [0, 0, 5, 0, 9, 0, 0, 8, 6, 6],
                         [8, 5, 0, 0, 8, 0, 0, 5, 7, 5],
                         [9, 9, 0, 0, 0, 0, 0, 0, 3, 9],
                         [9, 7, 0, 0, 0, 0, 0, 0, 4, 1],
                         [9, 9, 3, 5, 0, 8, 0, 0, 6, 3],
                         [7, 7, 1, 2, 3, 0, 0, 0, 0, 0],
                         [7, 9, 1, 1, 2, 5, 0, 0, 0, 9],
                         [2, 2, 1, 1, 1, 3, 0, 0, 0, 0],
                         [0, 4, 2, 1, 1, 2, 5, 0, 0, 0],
                         [0, 0, 2, 1, 1, 1, 9, 0, 0, 0]
                       ]
                   )
      applyN 100 step (0, grid2)
        `shouldBe` (1656,
                     toOctopusGrid
                       [ [0, 3, 9, 7, 6, 6, 6, 8, 6, 6],
                         [0, 7, 4, 9, 7, 6, 6, 9, 1, 8],
                         [0, 0, 5, 3, 9, 7, 6, 9, 3, 3],
                         [0, 0, 0, 4, 2, 9, 7, 8, 2, 2],
                         [0, 0, 0, 4, 2, 2, 9, 8, 9, 2],
                         [0, 0, 5, 3, 2, 2, 2, 8, 7, 7],
                         [0, 5, 3, 2, 2, 2, 2, 9, 6, 6],
                         [9, 3, 2, 2, 2, 2, 8, 9, 6, 6],
                         [7, 9, 2, 2, 2, 8, 6, 8, 6, 6],
                         [6, 7, 8, 9, 9, 9, 8, 7, 6, 6]
                       ]
                   )
    it "Part 1" $ do
      answer1 <- solution1
      answer1 `shouldBe` 1793
    it "Part 2" $ do
      answer2 <- solution2
      answer2 `shouldBe` 247
