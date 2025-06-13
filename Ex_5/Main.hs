{-# LANGUAGE GHC2024 #-}
{-# OPTIONS_GHC -Wall -Werror #-}

module Main where

import Prelude hiding
  ( sum
  , product
  , maximum
  , minimum
  , length
  , null
  , elem
  , concatMap
  , unzip
  )

import Data.Monoid (Sum(..))
import HW5 
import MultiSet (fromList)
import Test.HUnit

-- Section 1: Foldable functions
section1Tests :: Test
section1Tests = TestList
  [ "fold Sum [1,2,3]" ~: Sum 6 ~=? fold [Sum 1, Sum 2, Sum 3 :: Sum Int]
  , "toList (Just 5)" ~: [5 :: Int] ~=? toList (Just 5)
  , "elem 3 in [1,2,3]" ~: True ~=? elem (3 :: Int) [1,2,3]
  , "find odd [2,4,5,6]" ~: Just 5 ~=? find odd ([2,4,5,6] :: [Int])
  , "length [1,2,3]" ~: 3 ~=? length ([1,2,3] :: [Int])
  , "null []" ~: True ~=? null ([] :: [Int])
  , "null [1]" ~: False ~=? null ([1] :: [Int])
  , "maximum [3,7,1]" ~: Just 7 ~=? maximum ([3,7,1] :: [Int])
  , "minimum [3,7,1]" ~: Just 1 ~=? minimum ([3,7,1] :: [Int])
  , "maxBy negate [1,2,3]" ~: Just 1 ~=? maxBy negate ([1,2,3] :: [Int])
  , "minBy (*2) [3,4,1]" ~: Just 1 ~=? minBy (*2) ([3,4,1] :: [Int])
  , "sum [1,2,3]" ~: 6 ~=? sum ([1,2,3] :: [Int])
  , "product [2,3,4]" ~: 24 ~=? product ([2,3,4] :: [Int])
  , "concatMap (\\x -> [x,x]) [1,2]" ~: ([1,1,2,2] :: [Int]) ~=? concatMap (\x -> [x,x]) ([1,2] :: [Int])
  , "fold Sum []" ~: Sum 0 ~=? fold ([] :: [Sum Int]) -- Empty list
  , "fold Sum [1]" ~: Sum 1 ~=? fold [Sum 1 :: Sum Int] -- Single element
  , "toList Nothing" ~: ([] :: [Int]) ~=? toList (Nothing :: Maybe Int) -- Empty Maybe
  , "elem 4 in [1,2,3]" ~: False ~=? elem (4 :: Int) [1,2,3] -- Element not present
  , "find even [1,3,5]" ~: Nothing ~=? find even ([1,3,5] :: [Int]) -- No matching element
  , "length []" ~: 0 ~=? length ([] :: [Int]) -- Empty list
  , "maximum []" ~: Nothing ~=? maximum ([] :: [Int]) -- Empty list
  , "minimum []" ~: Nothing ~=? minimum ([] :: [Int]) -- Empty list
  , "maxBy negate []" ~: Nothing ~=? maxBy negate ([] :: [Int]) -- Empty list
  , "minBy (*2) []" ~: Nothing ~=? minBy (*2) ([] :: [Int]) -- Empty list
  , "sum []" ~: 0 ~=? sum ([] :: [Int]) -- Empty list
  , "product []" ~: 1 ~=? product ([] :: [Int]) -- Empty list
  , "concatMap (\\x -> [x,x]) []" ~: ([] :: [Int]) ~=? concatMap (\x -> [x,x]) ([] :: [Int]) -- Empty list
  ]

-- Section 2: Fold runners
run :: Foldable t => Fold a b c -> t a -> c
run = runFold

section2Tests :: Test
section2Tests = TestList
  [ "sumF" ~: 10 ~=? run sumF ([1,2,3,4] :: [Int])
  , "productF" ~: 24 ~=? run productF ([1,2,3,4] :: [Int])
  , "lengthF" ~: 4 ~=? run lengthF ([10,20,30,40] :: [Int])
  , "averageF" ~: 2.5 ~=? run averageF ([1,2,3,4] :: [Double])
  , "topKF 2" ~: [3, 5] ~=? run (topKF 2) ([1,3,5,2] :: [Int])
  , "nullF empty" ~: True ~=? run nullF ([] :: [Int])
  , "nullF nonempty" ~: False ~=? run nullF ([0] :: [Int])
  , "findF even" ~: Just 6 ~=? run (findF even) ([1,3,5,6,7] :: [Int])
  , "filterF even + sumF" ~: 6 ~=? run (filterF even sumF) ([1..4] :: [Int])
  , "mapF (*2) + sumF" ~: 12 ~=? run (mapF (*2) sumF) ([1,2,3] :: [Int])
  ,"sumF empty" ~: 0 ~=? run sumF ([] :: [Int]) -- Empty list
  , "productF empty" ~: 1 ~=? run productF ([] :: [Int]) -- Empty list
  , "lengthF empty" ~: 0 ~=? run lengthF ([] :: [Int]) -- Empty list
  , "averageF empty" ~: 0 ~=? run averageF ([] :: [Double]) -- Empty list
  , "topKF 0" ~: [] ~=? run (topKF 0) ([1,3,5,2] :: [Int]) -- Top 0 elements
  , "filterF odd + sumF" ~: 9 ~=? run (filterF odd sumF) ([1..5] :: [Int]) -- Filter odd numbers
  , "mapF (+1) + productF" ~: 120 ~=? run (mapF (+1) productF) ([1,2,3,4] :: [Int]) -- Map and product
  ]

-- Section 3: Functor utilities
section3Tests :: Test
section3Tests = TestList
  [ "fmapToFst" ~: [(97 :: Int, 'a'), (98 :: Int, 'b')] ~=? fmapToFst fromEnum ['a', 'b']
  , "fmapToSnd" ~: [('a', 97 :: Int), ('b', 98 :: Int)] ~=? fmapToSnd fromEnum ['a', 'b']
  , "strengthenL" ~: [('x', 1 :: Int), ('x', 2 :: Int)] ~=? strengthenL 'x' [1, 2]
  , "strengthenR" ~: [(1 :: Int, 'x'), (2 :: Int, 'x')] ~=? strengthenR 'x' [1, 2]
  , "unzip Maybe" ~: (Just (1 :: Int), Just (10 :: Int)) ~=? unzip (Just (1, 10))
  , "fmapToFst empty" ~: ([] :: [(Int, Char)]) ~=? fmapToFst fromEnum ([] :: [Char]) -- Empty list
  , "fmapToSnd empty" ~: ([] :: [(Char, Int)]) ~=? fmapToSnd fromEnum ([] :: [Char]) -- Empty list
  , "strengthenL empty" ~: ([] :: [(Char, Int)]) ~=? strengthenL 'x' ([] :: [Int]) -- Empty list
  , "strengthenR empty" ~: ([] :: [(Int, Char)]) ~=? strengthenR 'x' ([] :: [Int]) -- Empty list
  , "unzip Nothing" ~: (Nothing, Nothing) ~=? unzip (Nothing :: Maybe (Int, Int)) -- Empty Maybes
  ]

-- Section 4: coUnzip
section4Tests :: Test
section4Tests = TestList
    [ "coUnzip Left" ~: [Left 1, Left 2] ~=? (coUnzip (Left [1,2]) :: [Either Int Char])
    , "coUnzip Right" ~: [Right 'a', Right 'b'] ~=? (coUnzip (Right ['a','b']) :: [Either Int Char])
    , "coUnzip Left empty" ~: ([] :: [Either Int Char]) ~=? coUnzip (Left [] :: Either [Int] [Char]) -- Empty Left
    , "coUnzip Right empty" ~: ([] :: [Either Int Char]) ~=? coUnzip (Right [] :: Either [Int] [Char]) -- Empty Right
    , "coUnzip mixed" ~: [Left 1, Right 'a'] ~=? coUnzip (Left [1] :: Either [Int] [Char]) ++ coUnzip (Right ['a'] :: Either [Int] [Char]) -- Mixed values
  ]
-- Section 5: MultiSet Foldable instances
section5Tests :: Test
section5Tests = TestList
  [ "FoldOccur Sum [1,2,2,3,3,3]" ~: Sum 14 ~=? foldMap Sum (FoldOccur $ fromList ([1, 2, 2, 3, 3, 3] :: [Int]))
  , "MinToMax Sum [3,1,2,2,3,1]" ~: Sum 12 ~=? foldMap Sum (MinToMax $ fromList ([3, 1, 2, 2, 3, 1] :: [Int]))
  , "MaxToMin Sum [3,1,2,2,3,1]" ~: Sum 12 ~=? foldMap Sum (MaxToMin $ fromList ([3, 1, 2, 2, 3, 1] :: [Int]))
  , "FoldOccur toList [1,2,2,3,3,3]" ~: [1,2,2,3,3,3] ~=? toList (FoldOccur $ fromList ([1, 2, 2, 3, 3, 3] :: [Int]))
  , "MinToMax toList [3,1,2,2,3,1]" ~: [1,1,2,2,3,3] ~=? toList (MinToMax $ fromList ([3, 1, 2, 2, 3, 1] :: [Int]))
  , "MaxToMin toList [3,1,2,2,3,1]" ~: [3,3,2,2,1,1] ~=? toList (MaxToMin $ fromList ([3, 1, 2, 2, 3, 1] :: [Int]))
  , "FoldOccur Sum empty" ~: Sum 0 ~=? foldMap Sum (FoldOccur $ fromList ([] :: [Int])) -- Empty MultiSet
  , "MinToMax Sum empty" ~: Sum 0 ~=? foldMap Sum (MinToMax $ fromList ([] :: [Int])) -- Empty MultiSet
  , "MaxToMin Sum empty" ~: Sum 0 ~=? foldMap Sum (MaxToMin $ fromList ([] :: [Int])) -- Empty MultiSet
  , "FoldOccur toList empty" ~: [] ~=? toList (FoldOccur $ fromList ([] :: [Int])) -- Empty MultiSet
  , "MinToMax toList empty" ~: [] ~=? toList (MinToMax $ fromList ([] :: [Int])) -- Empty MultiSet
  , "MaxToMin toList empty" ~: [] ~=? toList (MaxToMin $ fromList ([] :: [Int])) -- Empty MultiSet
  , "FoldOccur Sum single" ~: Sum 1 ~=? foldMap Sum (FoldOccur $ fromList ([1] :: [Int])) -- Single element
  , "MinToMax Sum single" ~: Sum 1 ~=? foldMap Sum (MinToMax $ fromList ([1] :: [Int])) -- Single element
  , "MaxToMin Sum single" ~: Sum 1 ~=? foldMap Sum (MaxToMin $ fromList ([1] :: [Int])) -- Single element
  ]

allTests :: Test
allTests = TestList
  [ section1Tests
  , section2Tests
  , section3Tests
  , section4Tests
  , section5Tests
  ]

main :: IO ()
main = do
  _ <- runTestTT allTests
  return ()
