-- Yossi Partouche
-- Dana Stok
{-# LANGUAGE GHC2024 #-}
-- Implement the following functions.
-- When you're done, ghc -Wall -Werror HW3.hs should successfully compile.
{-# OPTIONS_GHC -Wall -Werror #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module HW3 where

import Prelude (Bool (..), Char, Either (..), Enum (..), Eq (..), Int, Integer, Maybe (..), Num (..), Ord (..), Rational, Show (..), String, all, and, any, concat, concatMap, const, curry, div, divMod, drop, dropWhile, elem, error, even, filter, flip, foldr, fst, head, id, init, last, length, lines, lookup, map, maximum, minimum, mod, not, notElem, null, odd, or, otherwise, product, reverse, show, snd, splitAt, sum, tail, take, takeWhile, uncurry, undefined, unlines, unwords, unzip, words, zip, zipWith, (!!), ($), (&&), (++), (.), (/), (||), fromIntegral)

import Data.Either (either, fromLeft, fromRight, isLeft, isRight, lefts, partitionEithers, rights)
import Data.Enum (Bounded)
import Data.List (find, foldl', isInfixOf, isPrefixOf, isSuffixOf, nub, uncons)
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, listToMaybe, mapMaybe, maybe)
import Data.Ratio (denominator, numerator, (%))
import qualified Data.Set as Set 
import Data.Bits (Bits(xor))
import Data.Char (intToDigit)
import GHC.Internal.Text.Read (Lexeme(String))
import Data.Graph (path)

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
itoList (x :> xs) = x : itoList xs
iiterate :: (a -> a) -> a -> InfiniteList a
iiterate f x = x :> iiterate f (f x)
irepeat :: a -> InfiniteList a
irepeat x = iiterate id x 

naturals :: InfiniteList Integer
naturals = iiterate (\x -> x+1) 0 

imap :: (a -> b) -> InfiniteList a -> InfiniteList b
imap f (x :> xs) = f x :> imap f xs

iconcat :: InfiniteList [a] -> InfiniteList a
iconcat (fs :> sn :> xs) = addToInf (fs ++ sn) (iconcat xs) 
    where 
        addToInf :: [a] -> InfiniteList a -> InfiniteList a
        addToInf [] y = y
        addToInf (x:rest) y = x :> addToInf rest y

grouped :: Integer -> InfiniteList a -> InfiniteList [a]
grouped n xs =  sampleN (fromIntegral n) xs :> grouped n (skip n xs)

skip :: Integer -> InfiniteList a -> InfiniteList a
skip 0 y = y
skip m (_:>ys) = skip (m-1) ys

mergeConcat :: [a] -> InfiniteList a -> InfiniteList a 
mergeConcat [] rest = rest
mergeConcat (y:ys) rest = y :> (mergeConcat ys rest)

reverseN :: Integer -> InfiniteList a -> InfiniteList a
reverseN 0 xs =  xs 
reverseN n xs = mergeConcat (revExt n xs) (skip n xs) 
    where 
        revExt :: Integer -> InfiniteList a -> [a]
        revExt m = reverse . sampleN (fromIntegral m)



sqrtInf :: Rational -> InfiniteList Rational
sqrtInf r = go r 
    where 
        go :: Rational -> InfiniteList Rational
        go current = (current) :> go ((current + r / current) / 2)

type InfiniteString = InfiniteList Char

longDivision :: Rational -> InfiniteString
longDivision r = 
    let n = numerator r 
        d = denominator r 
    in if r < 0  then '-' :> longDivision (-r) else integerPart n d
    where 
        integerPart :: Integer -> Integer -> InfiniteString
        integerPart num denom = mergeConcat (intToDigitM (num `div` denom)) ('.' :> decimalPart denom (num `mod` denom))

            where
                decimalPart :: Integer -> Integer -> InfiniteString
                decimalPart denom' val  = 
                    mergeConcat (intToDigitM ((val*10) `div` denom'))  (decimalPart denom' ((val*10) `mod` denom'))

                intToDigitM:: Integer -> [Char]
                intToDigitM n 
                    | n < 10 = [toDigit n]
                    | otherwise = intToDigitM (n `div` 10) ++ [toDigit (n `mod` 10)]

toDigit :: Integer -> Char
toDigit x = toEnum (fromEnum '0' + fromIntegral x)


sqrtStrings :: Rational -> InfiniteList (InfiniteList Char)
sqrtStrings n = decimalise (sqrtInf n)
    where
        decimalise :: InfiniteList Rational -> InfiniteList (InfiniteList Char)
        decimalise (x :> xs) = longDivision x :> decimalise xs

-- Section 3: Maze
data Cell = Open | Blocked | Treasure deriving (Show, Eq, Ord)
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
            |layout maze !! row !! col == Treasure = Right (CellPosition row col)
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
isEmpty :: Queue a -> Bool
isEmpty (Queue [] []) = True
isEmpty _ = False

type Path a = [a]
type BFSQueue a = Queue (Path a)

shortestPath :: Maze -> CellPosition -> CellPosition -> Either Error [CellPosition]
shortestPath maze startPos destPos = 
    case getAvailableMoves maze startPos of
        Left err -> Left err
        Right _ -> case getAvailableMoves maze destPos of
            Left err -> Left err
            Right _ -> case runBFS maze destPos (enqueue startPos emptyQueue) (Set.insert startPos Set.empty) ([]) of
                Left err -> Left err
                Right fullPath -> Right (stripEnds fullPath)
    where
        stripEnds (_:xs@(_:_)) = init xs
        stripEnds _ = []

runBFS :: Maze -> CellPosition -> Queue CellPosition -> Set.Set CellPosition -> [(CellPosition, CellPosition)] -> Either Error [CellPosition]
runBFS maze destPos toVisit visited fromTo = case dequeue toVisit of 
    Nothing -> Left NoPath
    Just (currCell, restQueue) -> if currCell == destPos then Right (getPath fromTo destPos)
    else 
        case getAvailableMoves maze currCell of
            Left err -> Left err
            Right pMoves -> 
                let newMoves = (filteredList pMoves visited) 
                    newEdges = fromTo ++ map (\next -> (currCell, next)) newMoves
                    newToVisit = foldr enqueue restQueue newMoves
                    newVisited = foldr Set.insert visited newMoves
                in runBFS maze destPos newToVisit newVisited newEdges

getPath :: [(CellPosition, CellPosition)] -> CellPosition -> [CellPosition]
getPath edges curr = 
    case find (\(_, to) -> to == curr) edges of
        Nothing -> [curr]
        Just (from, _) -> getPath edges from ++ [curr]

filteredList :: [CellPosition] -> Set.Set CellPosition -> [CellPosition]
filteredList xs visited = filter (`Set.notMember` visited) xs

        
-- Bonus (15 points)
treasureHunt :: Maze -> CellPosition -> Either Error [CellPosition]
treasureHunt maze startPos = 
    case getAvailableMoves maze startPos of
        Left err -> Left err
        Right _ -> 
            let treasureList = findTreasures maze 0 0
            in case createFullPath startPos treasureList of
                Left err -> Left err
                Right [] -> Right []
                Right (_:xs) -> Right xs
    where
        createFullPath :: CellPosition -> [CellPosition] -> Either Error [CellPosition]
        createFullPath _ [] = Right []
        createFullPath current (tCell:rest) = 
            case runBFS maze tCell (enqueue current emptyQueue) (Set.insert current Set.empty) ([]) of
                Left err -> Left err
                Right pathToT -> 
                    case createFullPath tCell rest of
                        Left err -> Left err
                        Right [] -> Right pathToT
                        Right (_:restTail) -> Right (pathToT ++ restTail)


findTreasures :: Maze ->Int -> Int -> [CellPosition]
findTreasures maze h w 
    | h == height maze = []
    | w == width maze = findTreasures maze (h+1) 0
    | cellAt maze (CellPosition h w) == Just Treasure = (CellPosition h w) : findTreasures maze h (w+1)
    | otherwise = findTreasures  maze h (w+1)

