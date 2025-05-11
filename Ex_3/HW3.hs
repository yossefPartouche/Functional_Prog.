{-# LANGUAGE GHC2024 #-}
-- Implement the following functions.
-- When you're done, ghc -Wall -Werror HW3.hs should successfully compile.
{-# OPTIONS_GHC -Wall -Werror #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module HW3 where

import Prelude (Bool (..), Char, Either (..), Enum (..), Eq (..), Int, Integer, Maybe (..), Num (..), Ord (..), Rational, Show (..), String, all, and, any, concat, concatMap, const, curry, div, divMod, drop, dropWhile, elem, error, even, filter, flip, foldr, fst, head, id, init, last, length, lines, lookup, map, maximum, minimum, mod, not, notElem, null, odd, or, otherwise, product, reverse, show, snd, splitAt, sum, tail, take, takeWhile, uncurry, undefined, unlines, unwords, unzip, words, zip, zipWith, (!!), ($), (&&), (++), (.), (/), (||))

import Data.Either (either, fromLeft, fromRight, isLeft, isRight, lefts, partitionEithers, rights)
import Data.Enum (Bounded)
import Data.List (find, foldl', isInfixOf, isPrefixOf, isSuffixOf, nub, uncons)
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, listToMaybe, mapMaybe, maybe)
import Data.Ratio (denominator, numerator, (%))
import qualified Data.Set as Set 

data Tree a = Empty | Tree (Tree a) a (Tree a) deriving (Show, Eq)

treeSize :: Tree a -> Int
treeSize = \case 
    Empty -> 0
    Tree l _ r -> 1 + treeSize l + treeSize r

treeHeight :: Tree a -> Int
treeHeight = \case 
    Empty -> 0
    Tree l _ r -> maximum[1 + treeHeight l, 1 + treeHeight r]

preOrderTraversal :: Tree a -> [a]
preOrderTraversal = \case
    Empty -> []
    Tree l cur r -> [cur] ++ preOrderTraversal l ++ preOrderTraversal r 


inOrderTraversal :: Tree a -> [a]
inOrderTraversal = \case
    Empty -> []
    Tree l cur r -> inOrderTraversal l ++ [cur] ++ inOrderTraversal r 

postOrderTraversal :: Tree a -> [a]
postOrderTraversal = \case
    Empty -> []
    Tree l cur r -> postOrderTraversal l ++ postOrderTraversal r ++ [cur]

data Classification = Full | Complete | FullAndComplete | Perfect | Degenerate | Other deriving (Show, Eq)
classify :: Tree a -> Classification
classify t  
    | isPerfect t = Perfect
    | isFull t && isComplete t = FullAndComplete
    | isFull t = Full
    | isComplete t = Complete
    | isDegenerate t = Degenerate
    | otherwise = Other

-- For classification
isFull :: Tree a -> Bool
isFull = \case
    Empty -> True 
    Tree Empty _ Empty -> True
    Tree _ _ Empty -> False 
    Tree Empty _ _ -> False
    Tree l _ r -> isFull l && isFull r
   
-- Helper function for isComplete
levelOrderTraversal :: Tree a -> [Maybe a]
levelOrderTraversal tree = levelOrderHelper [tree]
  where
    levelOrderHelper [] = []
    levelOrderHelper (Empty:xs) = Nothing : levelOrderHelper xs 
    levelOrderHelper (Tree l cur r:xs) = Just cur : levelOrderHelper (xs ++ [l, r])

-- For classification
isComplete :: Tree a -> Bool
isComplete t = helperChecker (levelOrderTraversal t)
  where
    helperChecker [] = True
    helperChecker (Nothing:xs) = all isNothing xs 
    helperChecker (_:xs) = helperChecker xs  

-- For classification
isPerfect :: Tree a -> Bool
isPerfect = \case
    Empty -> True
    Tree l _ r -> treeHeight l == treeHeight r && isPerfect l && isPerfect r 

-- For classification
isDegenerate :: Tree a -> Bool
isDegenerate = \case
    Empty -> True 
    Tree Empty _ r -> isDegenerate r 
    Tree l _ Empty -> isDegenerate l 
    Tree _ _ _ -> False

isBalanced :: Tree a -> Bool
isBalanced = \case
    Empty -> True
    Tree l _ r -> abs (treeHeight l - treeHeight r) <= 1 && isBalanced l && isBalanced r

-- Section 2: Infinite lists
data InfiniteList a = a :> InfiniteList a
infixr 5 :>

-- As defined in the PDf
sample :: InfiniteList a -> [a]
sample = sampleN 10
smallSample :: InfiniteList a -> [a]
smallSample = sampleN 5
sampleN :: Int -> InfiniteList a -> [a]
sampleN n = take n . itoList

itoList :: InfiniteList a -> [a]
itoList = undefined
iiterate :: (a -> a) -> a -> InfiniteList a
iiterate = undefined
irepeat :: a -> InfiniteList a
irepeat = undefined

naturals :: InfiniteList Integer
naturals = undefined 
imap :: (a -> b) -> InfiniteList a -> InfiniteList b
imap = undefined
iconcat :: InfiniteList [a] -> InfiniteList a
iconcat = undefined
grouped :: Integer -> InfiniteList a -> InfiniteList [a]
grouped = undefined
reverseN :: Integer -> InfiniteList a -> InfiniteList a
reverseN = undefined

sqrtInf :: Rational -> InfiniteList Rational
sqrtInf = undefined
type InfiniteString = InfiniteList Char
longDivision :: Rational -> InfiniteString
longDivision = undefined
sqrtStrings :: Rational -> InfiniteList InfiniteString
sqrtStrings = undefined

-- Section 3: Maze
data Cell = Open | Blocked | Gold deriving (Show, Eq, Ord)
data Maze = Maze {width :: Int, height :: Int, layout :: [[Cell]]} deriving (Show)
cellAt :: Maze -> CellPosition -> Maybe Cell
cellAt maze (CellPosition h w)
    | h < 0 || h >= height maze = Nothing
    | w < 0 || w >=  width maze = Nothing
    | otherwise = Just (layout maze !! h !! w)

data CellPosition = CellPosition {row :: Int, col :: Int} deriving (Show, Eq, Ord)

data Error = OutOfBounds | InvalidCell | NoPath deriving (Show, Eq, Ord)
getAvailableMoves :: Maze -> CellPosition -> Either Error [CellPosition]
getAvailableMoves maze (CellPosition h w) = 
    if h < 0 || h >= height maze || w < 0 || w >= width maze then Left OutOfBounds
    else if layout maze !! h !! w == Blocked then Left InvalidCell
    else Right (rights (map cellContains [CellPosition h (w - 1), CellPosition h (w + 1), CellPosition (h - 1) w, CellPosition (h + 1) w]))
    where 
        cellContains (CellPosition row col)
            |row < 0 || row >= height maze || col < 0 || col >= width maze = Left OutOfBounds
            |layout maze !! row !! col == Blocked = Left InvalidCell 
            |layout maze !! row !! col == Gold = Right (CellPosition row col)
            |layout maze !! row !! col == Open = Right (CellPosition row col)
            |otherwise = Left InvalidCell 


-- From the lectures
data Queue a = Queue [a] [a] deriving (Show, Eq)
emptyQueue :: Queue a
emptyQueue = Queue [] []
enqueue :: a -> Queue a -> Queue a
enqueue a (Queue l r) = Queue (a : l) r
dequeue :: Queue a -> Maybe (a, Queue a)
dequeue (Queue [] []) = Nothing
dequeue (Queue l []) = dequeue $ Queue [] (reverse l)
dequeue (Queue l (r : rs)) = Just (r, Queue l rs)

shortestPath :: Maze -> CellPosition -> CellPosition -> Either Error [CellPosition]
shortestPath = undefined

-- Bonus (15 points)
treasureHunt :: Maze -> CellPosition -> Either Error [CellPosition]
treasureHunt = undefined
