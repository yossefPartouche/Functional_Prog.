{-# LANGUAGE GHC2024 #-}
{-# OPTIONS_GHC -Wall -Werror #-}

module Main where

import Test.HUnit
import HW5

-- Helper to run Fold-based tests
run :: Foldable t => Fold a b c -> t a -> c
run = runFold

hw5Tests :: Test
hw5Tests = TestList
  [ "sumF over [1,2,3]" ~: 6 ~=? run sumF [1,2,3]
  , "productF over [1,2,3,4]" ~: 24 ~=? run productF [1,2,3,4]
  , "lengthF over \"abcde\"" ~: 5 ~=? run lengthF "abcde"
  , "averageF over [1,2,3,4]" ~: 2.5 ~=? run averageF [1,2,3,4 :: Double]
  , "topKF 3 over [5,1,3,9,7]" ~: [3,7,9] ~=? run (topKF 3) [5,1,3,9,7]
  , "nullF over []" ~: True ~=? run nullF ([] :: [Int])
  , "nullF over [1]" ~: False ~=? run nullF [1]
  , "findF even over [1,3,4,6]" ~: Just 4 ~=? run (findF even) [1,3,4,6]
  , "filterF even + sumF over [1..5]" ~: 6 ~=? run (filterF even sumF) [1..5]
  , "mapF (*2) + sumF over [1,2,3]" ~: 12 ~=? run (mapF (*2) sumF) [1,2,3]
  ]

main :: IO ()
main = do
  _ <- runTestTT hw5Tests
  return ()
