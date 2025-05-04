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
import Distribution.Simple.Utils (xargs)

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
    f (Right y) (l, r) = ( l, y : r)

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
  Plus e1 e2 -> addParIfNeed e1 ++ " + " ++ addParIfNeed e2
  Minus e1 e2 -> addParIfNeed e1 ++ " - " ++ addParIfNeed e2
  Mul e1 e2 -> addParIfNeed e1 ++ " * " ++ addParIfNeed e2
  Div e1 e2 -> addParIfNeed e1 ++ " / " ++ addParIfNeed e2
  where
    addParIfNeed exp = case exp  of
      Iden _ -> exprToString exp
      Lit _ -> exprToString exp
      _ -> "(" ++ exprToString exp ++ ")"


-- Bonus (25 points): Same as the above, but without unnecessary parentheses
exprToString' :: Expr -> String
exprToString' = undefined
-- Returns Nothing on division by zero.
partialEvaluate :: [(String, Int)] -> Expr -> Maybe Expr
partialEvaluate env = \case
  Iden x -> case lookup x env of
    Just val -> Just (Lit val)
    Nothing  -> Just (Iden x)
  Lit n -> Just (Lit n)
  Plus e1 e2 -> getEval (+) Plus False e1 e2
  Minus e1 e2 -> getEval (-) Minus False e1 e2
  Mul e1 e2 -> getEval (*) Mul False e1 e2
  Div e1 e2 -> getEval (div) Div True e1 e2
  where
    getEval f op isDiv e1 e2 = do
      v1 <- partialEvaluate env e1
      v2 <- partialEvaluate env e2
      case v2 of 
        Lit 0 -> if isDiv then Nothing else continue v1 v2
        _ -> continue v1 v2
      where 
        continue v1 v2 = case (v1, v2) of
          (Lit n1, Lit n2) -> Just (Lit (f n1 n2))
          _ -> Just (op v1 v2)

negateExpr :: Expr -> Expr
negateExpr exp = Mul (Lit (-1)) exp


-- if the exponent is smaller than 0, this should return 0.
powerExpr :: Expr -> Int -> Expr
powerExpr exp n
  | n < 0 = investigate 0
  | n == 0 = investigate 1
  | n == 1 = exp
  | otherwise = Mul exp (powerExpr exp (n - 1))
  where 
    investigate :: Int -> Expr
    investigate pow = case partialEvaluate [] exp of 
      Nothing -> Div (Lit 1) (Lit 0)  -- A trap! Will later evaluate to Nothing!! 
      Just _ -> if pow == 1 then Lit 1 else Lit 0



modExpr :: Expr -> Expr -> Expr
modExpr exp1 exp2 = Minus exp1 (Mul exp2 (Div exp1 exp2))

-- Section 3.1: zips and products
zip :: [a] -> [b] -> [(a, b)]
zip [] _ = []
zip _ [] = []
zip (x : xs) (y : ys) = (x,y) : zip xs ys

zipWithIndex :: [a] -> [(Int, a)]
zipWithIndex = counter 1
  where
    counter _ [] = []
    counter i (x:xs) = (i, x) : counter (i+1) xs

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith _ [] _ = [] 
zipWith _ _ [] = []
zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys

zipWithDefault :: a -> b -> [a] -> [b] -> [(a, b)] -- Zips two lists, filling missing elements with defaults
zipWithDefault _ _ [] _ = []
zipWithDefault _ _ _ [] = []
zipWithDefault dx dy (x:xs) (y:ys) = (x,y) : zipRest xs ys
  where 
    zipRest [] ys' = map ((,) dx) ys'
    zipRest xs' [] = map (flip (,) dy) xs'
    zipRest (x':xs') (y':ys') = (x', y') : zipRest xs' ys'

data ZipFail = ErrorFirst | ErrorSecond deriving (Eq, Show)

zipEither :: [a] -> [b] -> Either ZipFail [(a, b)]
zipEither _ [] = Left ErrorSecond
zipEither [] _ = Left ErrorFirst
zipEither (x:xs) (y:ys) = 
  case zipEither xs ys of 
    Right pairs -> Right ((x,y) : pairs)
    Left err -> Left err

unzip :: [(a, b)] -> ([a], [b])
unzip [] = ([], [])
unzip ((x,y):rest) = 
  let (xs, ys) = unzip rest
  in (x:xs, y:ys)
{-
- Debatting if this is more declarative
unzip :: [(a,b)] -> ([a], [b])
unzip tupledList = go tupledList ([], [])
  where
    go [] (as, bs) = (as, bs)
    go ((x,y): rest) (as, bs) = go rest (as ++[x], bs ++ [u])
-}

unzipFirst :: [(a, b)] -> [a]
unzipFirst [] = []
unzipFirst ((x,_): rest) = 
  let xs = unzipFirst rest
  in x:xs

unzipSecond :: [(a, b)] -> [b]
unzipSecond [] = []
unzipSecond ((_, y): rest) = 
  let ys = unzipSecond rest
  in y:ys

cartesianWith :: (a -> b -> c) -> [a] -> [b] -> [c]
cartesianWith _ [] _ = []
cartesianWith _ _ [] = []
cartesianWith f (x:xs) (y:ys) = go f x (y:ys) ++ cartesianWith f xs ys
  where 
    go _ _ [] = []
    go f' x' (y':ys') = f' x' y' : go f' x' ys'




-- Section 3.2: list functions
snoc :: [a] -> a -> [a] -- The opposite of cons
snoc [] x = [x]
snoc (head : tail) x = head : snoc tail x

{- Debatting if this is more declarative
unzip :: [(a,b)] -> ([a], [b])
unzip tupledList = go tupledList ([], [])
  where
    go [] (as, bs) = (as, bs)
    go ((x,y): rest) (as, bs) = go rest (as ++[x], bs ++ [u])
-}
take :: Int -> [a] -> [a]
-- The last element of the result (if non-empty) is the last element which satisfies the predicate
take n xs
  | n <= 0 = []
  | otherwise = go n xs
  where   
    go 0 _  = []
    go _ [] = []
    go m (x':xs') = x' : go (m-1) xs'

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile p (x:xs)
  | p x = x : takeWhile p xs
  | otherwise = []

drop :: Int -> [a] -> [a]
-- The first element of the result (if non-empty) is the first element which doesn't satisfy the predicate
drop n xs 
  | n <= 0 = xs
  | otherwise = go n xs
  where
    go 0 xs' = xs'
    go _ [] = []
    go m (_:xs') = go (m-1) xs'
  
dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ [] = []
dropWhile p (x:xs)
  | p x = dropWhile p xs
  | otherwise = (x:xs)

slice :: Int -> Int -> [a] -> [a]
slice _ _ [] = []
slice m n (x:xs) = take (n-m) $ drop m (x:xs) 

takeEvery :: Int -> [a] -> [a]
takeEvery _ [] = []
takeEvery n xs
  | n <= 0 = []
  | otherwise = go (drop (n-1) xs)
  where 
    go [] = []
    go (y:ys) = y : (go $ drop (n-1) ys)

dropEvery :: Int -> [a] -> [a]
dropEvery _ [] = []
dropEvery n xs
  | n <= 0 = xs
  | otherwise = go xs
  where 
    go [] = []
    go ys = take (n-1) ys ++ go (drop n ys)

nub :: [Int] -> [Int]
nub [] = []
nub (x:xs) = x : filter (/= x) xs

uniq :: [Int] -> [Int]
uniq [] = []
uniq (x:xs) = x : uniq (filter (/= x) xs)

-- Section 3.3: base64
base64Chars :: String -- As defined in the PDF
base64Chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

toBase64 :: Integer -> String
toBase64 n
  | n < 0     = '-' : toBase64 (-n)  -- Handle negative numbers with leading '-'
  | n == 0    = "A"                  -- Zero is represented as 'A' (index 0)
  | otherwise = encodeBase64 n ""    -- Use helper function for positive numbers
  where
    encodeBase64 :: Integer -> String -> String
    encodeBase64 0 acc = acc
    encodeBase64 m acc = 
      let
        idx = fromIntegral (m `mod` 64)
        char = base64Chars !! idx
        newAcc = if null acc then [char] else char : acc
      in encodeBase64 (m `div` 64) newAcc

fromBase64 :: String -> Maybe Integer
fromBase64 "" = Nothing                         
fromBase64 "-" = Nothing                       
fromBase64 ('-':str) = maybeMap negate (fromBase64 str) 
fromBase64 str = decodeBase64 str 0
  where
    decodeBase64 :: String -> Integer -> Maybe Integer
    decodeBase64 [] acc = Just acc              -- base case and sucessful case
    decodeBase64 (c:cs) acc =
      case findIndex c base64Chars of
        Just idx -> decodeBase64 cs (acc * 64 + fromIntegral idx) 
        Nothing  -> Nothing                    -- deals with all cases that aren't in base64Chars
    
    findIndex :: Char -> String -> Maybe Int
    findIndex _ [] = Nothing
    findIndex char (x:xs)
      | char == x = Just 0                      
      | otherwise = maybeMap (+1) (findIndex char xs)  
