import Test.HUnit
import Data.Map (fromList)
import Data.List (transpose)
import HW5


foldableTests :: Test
foldableTests = TestList
  [ "maximum of [1, 5, 3]" ~:
      Just 5 ~=? (maximum [1, 5, 3])

  , "minimum of [1, 5, 3]" ~:
      Just 1 ~=? (minimum [1, 5, 3])

  , "minBy negate [1, 2, 3]" ~:
      Just 3 ~=? (minBy negate [1, 2, 3])  -- because -3 is smallest

  , "maxBy length [\"hi\", \"hello\", \"bye\"]" ~:
      Just "hello" ~=? (maxBy length ["hi", "hello", "bye"])

  , "find even [1, 3, 4, 5]" ~:
      Just 4 ~=? (find even [1, 3, 4, 5])

  , "find (>10) [1, 3, 4, 5]" ~:
      Nothing ~=? (find (>10) [1, 3, 4, 5])

  , "sum of [1,2,3,4]" ~:
      10 ~=? sum [1,2,3,4]

  , "product of [1,2,3,4]" ~:
      24 ~=? product [1,2,3,4]

  , "concatMap replicate 2 [1,2]" ~:
      [1,1,2,2] ~=? concatMap (replicate 2) [1,2]

  , "null on []" ~:
      True ~=? null ([] :: [Int])

  , "null on [1]" ~:
      False ~=? null [1 :: Int]

  , "length on [1,2,3]" ~:
      3 ~=? length [1,2,3]
  ]



-- Run all tests
allTests :: Test
allTests = TestList
  [
    foldableTests
  ]

main :: IO ()
main = runTestTT allTests >> return ()