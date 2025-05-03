{-# LANGUAGE GHC2024 #-}
-- Tells HLS to show warnings, and the file won't be compiled if there are any warnings, e.g.,
-- eval (-- >>>) won't work.
{-# OPTIONS_GHC -Wall -Werror #-}
-- Refines the above, allowing for unused imports.
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE LambdaCase #-} --- Dana Added this checking if OK!

module HW2 where

import Data.List (find, foldl')
import Prelude (Bool (..), Bounded (..), Char, Either (..), Enum (..), Eq (..), Int, Integer, Maybe (..), Num (..), Ord (..), Show (..), String, all, and, any, concat, concatMap, const, curry, div, divMod, elem, error, even, filter, flip, foldl, foldr, fromIntegral, fst, id, length, lines, lookup, map, mod, not, notElem, null, odd, otherwise, product, snd, sum, uncurry, undefined, unlines, unwords, words, (!!), ($), (&&), (++), (.), (||))

------------------------------------------------
-- DO NOT MODIFY ANYTHING ABOVE THIS LINE !!! --
------------------------------------------------
-- Utilities from the lecture
maybeMap :: (a -> b) -> Maybe a -> Maybe b
maybeMap f = \case
  Nothing -> Nothing
  Just x -> Just (f x)

eitherMap :: (a -> b) -> Either e a -> Either e b
eitherMap f = \case
  Left x -> Left x
  Right y -> Right (f y)

-- Section 1.1: Basic Maybes
fromMaybe :: a -> Maybe a -> a
fromMaybe a = \case
  Nothing -> a 
  Just x -> x

concatMaybeMap :: (a -> Maybe b) -> Maybe a -> Maybe b
concatMaybeMap f = \case 
  Nothing -> Nothing
  Just x -> f x  

maybe :: b -> (a -> b) -> Maybe a -> b
maybe b f = \case 
  Nothing -> b
  Just x -> f x 

maybeHead :: [a] -> Maybe a
maybeHead = \case
  [] -> Nothing
  (x:_) -> Just x 

-- Helper function for maybeLast, maybeMaximum, maybeMinimum
iterateList :: (a -> a -> a) -> [a] -> Maybe a
iterateList _ [] = Nothing
iterateList f (x:xs) = Just (run x xs)
  where
    run curr = \case
      [] -> curr
      (val:tail) -> run (f curr val) tail

maybeLast :: [a] -> Maybe a
maybeLast xs = iterateList (\_ y -> y) xs
 
maybeMaximum :: [Int] -> Maybe Int
maybeMaximum xs = iterateList (\x y -> if x > y then x else y) xs

maybeMinimum :: [Int] -> Maybe Int
maybeMinimum xs = iterateList (\x y -> if x < y then x else y) xs

filterMaybe :: (a -> Bool) -> Maybe a -> Maybe a
filterMaybe f = \case
  Nothing -> Nothing
  Just x -> if f x then Just x else Nothing 

sumMaybe :: Maybe Int -> Maybe Int -> Maybe Int
sumMaybe x y = case (x, y) of
  (Just a, Just b) -> Just (a + b)
  _ -> Nothing

liftMaybe2 :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
liftMaybe2 f x y = case (x, y) of
  (Just a, Just b) -> Just (f a b)
  _ -> Nothing

catMaybes :: [Maybe a] -> [a]
catMaybes = \case
  [] -> []
  (Nothing: tail) -> catMaybes tail
  (Just val: tail) -> val : catMaybes tail

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe f = \case
  [] -> []
  (x:xs) -> case f x of 
      Nothing -> mapMaybe f xs
      Just val -> val : mapMaybe f xs


-- Section 1.2 Basic Eithers
fromEither :: b -> Either a b -> b
fromEither b = \case
  Left _ -> b
  Right x -> x
concatEitherMap :: (a -> Either e b) -> Either e a -> Either e b
concatEitherMap f = \case
  Left x -> Left x
  Right y -> f y 

either :: (a -> c) -> (b -> c) -> Either a b -> c
either f g = \case
  Left x -> f x
  Right y -> g y

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f = \case
  Left x -> Left (f x)
  Right y -> Right y  

catEithers :: [Either e a] -> Either e [a]
catEithers xs = run [] xs
  where 
    run :: [a] -> [Either e a] -> Either e [a]
    run lst [] = Right lst
    run _ (Left x:_) = Left x 
    run lst (Right y:tail) = run (lst ++ [y]) tail

mapEither :: (a -> Either e b) -> [a] -> Either e [b]
mapEither f xs = catEithers (map f xs)

partitionEithers :: [Either a b] -> ([a], [b]) 
partitionEithers = foldr f ([], [])
  where 
    f (Left x) (l, r) = (x : l, r)
    f (Right y) (l, r) = (l, y : r)

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = \case 
  Left _ -> Nothing
  Right y -> Just y

productEither :: (Either a Int -> Either a Int -> Either a Int)
productEither (Right x) (Right y) = Right (x * y)  
productEither (Left x) _ = Left x  
productEither _ (Left x) = Left x

liftEither2 :: (a -> b -> c) -> Either e a -> Either e b -> Either e c
liftEither2 f (Right x) (Right y) = Right (f x y)
liftEither2 _ (Left x) _ = Left x
liftEither2 _ _ (Left x) = Left x

-- Section 2: Expressions
data Expr = Iden String | Lit Int | Plus Expr Expr | Minus Expr Expr | Mul Expr Expr | Div Expr Expr deriving (Show, Eq)

-- Adds parentheses around sub-expressions (the top level expression never has parentheses).
exprToString :: Expr -> String
exprToString = \case
  Iden x -> x 
  Lit n -> show n
  Plus exp1 exp2 -> "(" ++ exprToString exp1 ++ " + " ++ exprToString exp2 ++ ")"
  Minus exp1 exp2 -> "(" ++ exprToString exp1 ++ " - " ++ exprToString exp2 ++ ")"
  Mul exp1 exp2 -> "(" ++ exprToString exp1 ++ " * " ++ exprToString exp2 ++ ")"
  Div exp1 exp2 -> "(" ++ exprToString exp1 ++ " / "  ++ exprToString exp2 ++ ")"


-- Bonus (25 points): Same as the above, but without unnecessary parentheses
exprToString' :: Expr -> String
exprToString' = undefined
-- Returns Nothing on division by zero.
partialEvaluate :: [(String, Int)] -> Expr -> Maybe Expr
partialEvaluate env = \case 
  Iden x -> case lookup x env of 
    Just val -> Just (Lit val)
    Nothing -> Nothing
  Lit n -> Just (Lit n)
  Plus exp1 exp2 -> do
    Lit v1 <- partialEvaluate env exp1
    Lit v2 <- partialEvaluate env exp2 
    Just (Lit (v1 + v2))
  Minus exp1 exp2 -> do
    Lit v1 <- partialEvaluate env exp1
    Lit v2 <- partialEvaluate env exp2 
    Just (Lit (v1 - v2))
  Mul exp1 exp2 -> do
    Lit v1 <- partialEvaluate env exp1 
    Lit v2 <- partialEvaluate env exp2 
    Just (Lit (v1 * v2))
  Div exp1 exp2 -> do
    Lit v1 <- partialEvaluate env exp1 
    Lit v2 <- partialEvaluate env exp2 
    if v2 == 0 then Nothing else Just (Lit (v1 `div` v2))


negateExpr :: Expr -> Expr
negateExpr = \case 
   Iden x -> Iden x  
   Lit n -> Lit (-n)  
   Plus exp1 exp2 -> 
       case (negateExpr exp1, negateExpr exp2) of
         (Lit v1, Lit v2) -> Lit (-(v1 + v2))  
         _ -> Plus exp1 exp2  
   Minus exp1 exp2 -> 
       case (negateExpr exp1, negateExpr exp2) of
         (Lit v1, Lit v2) -> Lit (-(v1 - v2)) 
         _ -> Minus exp1 exp2 
   Mul exp1 exp2 -> 
       case (negateExpr exp1, negateExpr exp2) of
         (Lit v1, Lit v2) -> Lit (-(v1 * v2)) 
         _ -> Mul exp1 exp2  
   Div exp1 exp2 ->
      case (negateExpr exp1, negateExpr exp2) of
        (Lit v1, Lit v2) -> if v2 == 0 then Div exp1 exp2 else Lit (-(v1 `div` v2))
        _ -> Div exp1 exp2  


-- if the exponent is smaller than 0, this should return 0.
powerExpr :: Expr -> Int -> Expr
powerExpr = undefined
modExpr :: Expr -> Expr -> Expr
modExpr = undefined

-- Section 3.1: zips and products
zip :: [a] -> [b] -> [(a, b)]
zip = undefined
zipWithIndex :: [a] -> [(Int, a)]
zipWithIndex = undefined
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith = undefined
zipWithDefault :: a -> b -> [a] -> [b] -> [(a, b)] -- Zips two lists, filling missing elements with defaults
zipWithDefault = undefined
data ZipFail = ErrorFirst | ErrorSecond deriving (Eq, Show)
zipEither :: [a] -> [b] -> Either ZipFail [(a, b)]
zipEither = undefined
unzip :: [(a, b)] -> ([a], [b])
unzip = undefined
unzipFirst :: [(a, b)] -> [a]
unzipFirst = undefined
unzipSecond :: [(a, b)] -> [b]
unzipSecond = undefined
cartesianWith :: (a -> b -> c) -> [a] -> [b] -> [c]
cartesianWith = undefined

-- Section 3.2: list functions
snoc :: [a] -> a -> [a] -- The opposite of cons!
snoc = undefined
take :: Int -> [a] -> [a]
-- The last element of the result (if non-empty) is the last element which satisfies the predicate
take = undefined
takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile = undefined
drop :: Int -> [a] -> [a]
-- The first element of the result (if non-empty) is the first element which doesn't satisfy the predicate
drop = undefined
dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile = undefined
slice :: Int -> Int -> [a] -> [a]
slice = undefined
takeEvery :: Int -> [a] -> [a]
takeEvery = undefined
dropEvery :: Int -> [a] -> [a]
-- Removes *consecutive* repeats
dropEvery = undefined
nub :: [Int] -> [Int]
-- Removes *all* repeats, consecutive or not.
nub = undefined
uniq :: [Int] -> [Int]
uniq = undefined
-- Section 3.3: base64
base64Chars :: String -- As defined in the PDF
base64Chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

toBase64 :: Integer -> String
toBase64 = undefined
fromBase64 :: String -> Maybe Integer
fromBase64 = undefined
