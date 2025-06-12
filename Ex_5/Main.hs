{-# LANGUAGE GHC2024 #-}
{-# OPTIONS_GHC -Wall -Werror #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Main where

import Test.HUnit
import qualified Prelude (length)
import HW5
import Data.Monoid (Sum(..))
--import Data.Maybe (isNothing)
import Prelude hiding (elem, length, null, maximum, minimum, sum, product, concatMap)

testList :: [Int]
testList = [1, 2, 3, 4, 5]

emptyList :: [Int]
emptyList = []

singleList :: [Int]
singleList = [42]

stringList :: [String]
stringList = ["hello", "world", "haskell"]

section1Tests :: Test
section1Tests = TestList
  [ "fold (Sum monoid)" ~:
      Sum 6 ~=? fold [Sum 1, Sum 2, Sum 3]

  , "fold (List monoid)" ~:
      "helloworld" ~=? fold ["hello", "world"]

  , "toList [1,2,3]" ~:
      [1, 2, 3, 4, 5] ~=? toList testList

  , "toList (Just 42)" ~:
      [42] ~=? toList (Just 42)

  , "toList Nothing" ~:
      ([] :: [Int]) ~=? toList (Nothing :: Maybe Int)

  , "elem 3 in [1..5]" ~:
      True ~=? elem 3 testList

  , "elem 6 in [1..5]" ~:
      False ~=? elem 6 testList

  , "elem 42 in Just 42" ~:
      True ~=? elem 42 (Just 42)

  , "elem 42 in Nothing" ~:
      False ~=? elem 42 (Nothing :: Maybe Int)

  , "find (>3) in [1..5]" ~:
      Just 4 ~=? find (> 3) testList

  , "find (>10) in [1..5]" ~:
      Nothing ~=? find (> 10) testList

  , "length [1..5]" ~:
      5 ~=? length testList

  , "length []" ~:
      0 ~=? length emptyList

  , "length Just 42" ~:
      1 ~=? length (Just 42)

  , "length Nothing" ~:
      0 ~=? length (Nothing :: Maybe Int)

  , "null [1..5]" ~:
      False ~=? null testList

  , "null []" ~:
      True ~=? null emptyList

  , "null (Just 42)" ~:
      False ~=? null (Just 42)

  , "null Nothing" ~:
      True ~=? null (Nothing :: Maybe Int)

  , "maximum [1..5]" ~:
      Just 5 ~=? maximum testList

  , "maximum []" ~:
      Nothing ~=? maximum emptyList

  , "maximum [42]" ~:
      Just 42 ~=? maximum singleList

  , "maxBy length" ~:
      Just "haskell" ~=? maxBy Prelude.length stringList

  , "minBy length" ~:
      Just "hello" ~=? minBy Prelude.length stringList

  , "sum [1..5]" ~:
      15 ~=? sum testList

  , "sum []" ~:
      0 ~=? sum emptyList

  , "sum Just 42" ~:
      42 ~=? sum (Just 42)

  , "product [1..5]" ~:
      120 ~=? product testList

  , "product []" ~:
      1 ~=? product emptyList

  , "product Just 42" ~:
      42 ~=? product (Just 42)

  , "concatMap (\\x -> [x, x])" ~:
      [1,1,2,2,3,3] ~=? concatMap (\x -> [x, x]) [1,2,3]

  , "concatMap (const [])" ~:
      ([] :: [Int]) ~=? concatMap (const []) testList
  ]

main :: IO ()
main = runTestTT section1Tests >> return ()