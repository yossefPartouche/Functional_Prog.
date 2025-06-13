{-# LANGUAGE GHC2024 #-}
{-# OPTIONS_GHC -Wall -Werror #-}

module Main where

import HW5
import Test.HUnit

-- Section 1: Foldable functions
section1Tests :: Test
section1Tests = TestList
  [ "fold Sum [1,2,3]" ~: Sum 6 ~=? fold [Sum 1, Sum 2, Sum 3]
  , "toList (Just 5)" ~: [5] ~=? toList (Just 5)
  , "elem 3 in [1,2,3]" ~: True ~=? elem 3 [1,2,3]
  , "find odd [2,4,5,6]" ~: Just 5 ~=? find odd [2,4,5,6]
  , "length [1,2,3]" ~: 3 ~=? length [1,2,3]
  , "null []" ~: True ~=? null ([] :: [Int])
  , "null [1]" ~: False ~=? null [1 :: Int]
  , "maximum [3,7,1]" ~: Just 7 ~=? maximum [3,7,1]
  , "minimum [3,7,1]" ~: Just 1 ~=? minimum [3,7,1]
  , "maxBy negate [1,2,3]" ~: Just 1 ~=? maxBy negate [1,2,3]
  , "minBy (*2) [3,4,1]" ~: Just 1 ~=? minBy (*2) [3,4,1]
  , "sum [1,2,3]" ~: 6 ~=? sum [1,2,3]
  , "product [2,3,4]" ~: 24 ~=? product [2,3,4]
  , "concatMap (\x -> [x,x]) [1,2]" ~: [1,1,2,2] ~=? concatMap (\x -> [x,x]) [1,2]
  ]

-- Section 2: Fold runners
run :: Foldable t => Fold a b c -> t a -> c
run = runFold

section2Tests :: Test
section2Tests = TestList
  [ "sumF" ~: 10 ~=? run sumF [1,2,3,4]
  , "productF" ~: 24 ~=? run productF [1,2,3,4]
  , "lengthF" ~: 4 ~=? run lengthF [10,20,30,40]
  , "averageF" ~: 2.5 ~=? run averageF [1,2,3,4 :: Double]
  , "topKF 2" ~: [3, 5] ~=? run (topKF 2) [1,3,5,2]
  , "nullF empty" ~: True ~=? run nullF ([] :: [Int])
  , "nullF nonempty" ~: False ~=? run nullF [0]
  , "findF even" ~: Just 6 ~=? run (findF even) [1,3,5,6,7]
  , "filterF even + sumF" ~: 6 ~=? run (filterF even sumF) [1..4]
  , "mapF (*2) + sumF" ~: 12 ~=? run (mapF (*2) sumF) [1,2,3]
  ]

-- Section 3: Functor utilities
section3Tests :: Test
section3Tests = TestList
  [ "fmapToFst" ~: [(1,'a'),(2,'b')] ~=? fmapToFst fromEnum ['a','b']
  , "fmapToSnd" ~: [('a',1),('b',2)] ~=? fmapToSnd fromEnum ['a','b']
  , "strengthenL" ~: [('x',1),('x',2)] ~=? strengthenL 'x' [1,2]
  , "strengthenR" ~: [(1,'x'),(2,'x')] ~=? strengthenR 'x' [1,2]
  , "unzip" ~: ([1,2],[10,20]) ~=? unzip (Just (1,10), Just (2,20))
  ]

-- Section 4: coUnzip
section4Tests :: Test
section4Tests = TestList
  [ "coUnzip Left" ~: [Left 1, Left 2] ~=? coUnzip (Left [1,2])
  , "coUnzip Right" ~: [Right 'a', Right 'b'] ~=? coUnzip (Right ['a','b'])
  ]

allTests :: Test
allTests = TestList
  [ section1Tests
  , section2Tests
  , section3Tests
  , section4Tests
  ]

main :: IO ()
main = do
  _ <- runTestTT allTests
  return ()
