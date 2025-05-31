{-# LANGUAGE GHC2024 #-}
-- Implement the following functions.
-- When you're done, ghc -Wall -Werror MultiSet.hs HW4.hs should successfully compile.
{-# OPTIONS_GHC -Wall -Werror #-}
-- Num Bool is an orphan instance (not defined where Num or Bool are defined), so we need to silence the warning.
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module HW4 where

import Data.Either
import Data.List (find, intercalate, transpose)
import Data.Map qualified as Map
import Data.Maybe
import MultiSet qualified as MS
import Prelude (Bool (..), Char, Double, Either (..), Eq (..), Int, Integer, Integral, Maybe (..), Monoid (..), Num (..), Ord (..), Semigroup (..), Show (..), String, all, const, div, drop, error, filter, foldl', foldr, id, init, iterate, length, lookup, map, mod, not, otherwise, product, replicate, reverse, sum, undefined, zip, zipWith, (!!), ($), (&&), (++), (.), (^), (||))
--import Distribution.Simple.Utils (xargs)



-- Section 2: JSON data and Jsonable typeclass
newtype JString = JString String deriving (Show, Eq)

data Json
  = JsonNull
  | JsonBool Bool
  | JsonString JString
  | JsonInt Integer
  | JsonDouble Double
  | JsonArray [Json]
  | JsonObject (Map.Map String Json)
  deriving (Show, Eq)

class Jsonable a where
  toJson :: a -> Json
  fromJson :: Json -> Maybe a

instance Jsonable Bool where 
  toJson v              = JsonBool v

  fromJson (JsonBool v) = Just v
  fromJson _            = Nothing

instance Jsonable JString where
  toJson s             = JsonString s

  fromJson (JsonString s) = Just s
  fromJson _           = Nothing

instance Jsonable Integer where
  toJson v             = JsonInt v

  fromJson (JsonInt v) = Just v
  fromJson _           = Nothing

instance Jsonable Double where 
  toJson d                = JsonDouble d

  fromJson (JsonDouble d) = Just d
  fromJson _              = Nothing 

instance (Jsonable a, Jsonable b) => Jsonable (a, b) where 
  toJson (a,b) = JsonArray [toJson a, toJson b]

  fromJson (JsonArray [a,b]) = case (fromJson a, fromJson b) of
    (Just x, Just y) -> Just (x,y)
    _                -> Nothing
  fromJson _ = Nothing

instance (Jsonable a, Jsonable b, Jsonable c) => Jsonable (a, b, c) where
  toJson (a,b,c) = JsonArray [toJson a, toJson b, toJson c]

  fromJson :: (Jsonable a, Jsonable b, Jsonable c) => Json -> Maybe (a, b, c)
  fromJson (JsonArray [a,b,c]) = case (fromJson a, fromJson b, fromJson c) of
    (Just x, Just y, Just z) -> Just (x,y,z)
    _                -> Nothing
  fromJson _ = Nothing 

--Hard
instance Jsonable a => Jsonable (Maybe a) where 

  toJson Nothing = JsonNull
  toJson (Just x)  = JsonObject (Map.singleton "Just" (toJson x))

  -- JsonNull is legitimate of Json it happens to be Nothing, so it's Just Nothing
  fromJson JsonNull = Just Nothing 
  fromJson (JsonObject k) = case Map.lookup "Just" k of 
    -- Jsonable object that isn't Null so we return Just that
    Just v  -> case fromJson v of
        Just x  -> Just (Just x)
        Nothing -> Nothing
    -- We didn't find a map
    Nothing -> Nothing 
  fromJson _ = Nothing 

-- Harder 
instance (Jsonable l, Jsonable r) => Jsonable (Either l r) where 
  toJson (Left x) = JsonObject (Map.singleton "Left" (toJson x))
  toJson (Right y) = JsonObject (Map.singleton "Right" (toJson y))

  fromJson (JsonObject k) = case Map.lookup "Left" k of
    Just v -> case fromJson v of 
      Just x  -> Just (Left x)
      Nothing -> Nothing 
    Nothing -> 
      case Map.lookup "Right" k of
        Just v' -> case (fromJson v') of
          Just x' -> Just (Right x')
          Nothing -> Nothing 
        Nothing -> Nothing
  fromJson _ = Nothing

instance Jsonable a => Jsonable [a] where
  toJson xs = JsonArray (map toJson xs)

  fromJson (JsonArray xs) = foldr combine (Just []) xs 
    where 
      combine x acc = case (fromJson x, acc) of
        (Just parsed, Just rest) -> Just (parsed : rest)
        _ -> Nothing
  fromJson _ = Nothing

data Matrix a = Matrix [[a]] deriving (Show, Eq)

instance (Jsonable a, Ord a) => Jsonable (MS.MultiSet a) where
  toJson ms = JsonArray (map toJson (MS.toList ms))
  fromJson (JsonArray xs) = convert xs []
    where
      convert [] acc = Just (MS.fromList (reverse acc))
      convert (j:js) acc =
        case fromJson j of
          Just x  -> convert js (x:acc)
          Nothing -> Nothing
  fromJson _ = Nothing
          



instance Jsonable a => Jsonable (Matrix a) where 
  toJson (Matrix rows) = JsonArray (map (JsonArray . map toJson) rows)

  fromJson (JsonArray jsonRows) = 
    case parseRows jsonRows of
      Just rows -> Just (Matrix rows)
      Nothing   -> Nothing
    where
      parseRows [] = Just []
      parseRows (JsonArray jsonCells : rest) =
        case parseCells jsonCells of
          Just row -> case parseRows rest of
                        Just rows -> Just (row : rows)
                        Nothing   -> Nothing
          Nothing -> Nothing
      parseRows (_ : _) = Nothing

      parseCells [] = Just []
      parseCells (j : js) =
        case fromJson j of
          Just x  -> case parseCells js of
                      Just xs -> Just (x : xs)
                      Nothing -> Nothing
          Nothing -> Nothing

  fromJson _ = Nothing


-- A sparse matrix is a more efficient representation of a matrix when most of the entries are zero.
-- Note that zero values should not appear in the map.
data SparseMatrix a
  = SparseMatrix
  { rows :: Integer
  , cols :: Integer
  , entries :: Map.Map (Integer, Integer) a
  }
  deriving (Show, Eq)

instance Jsonable a => Jsonable (SparseMatrix a) where
  toJson (SparseMatrix r c e) =
    JsonObject (Map.fromList
      [ ("rows", toJson r)
      , ("cols", toJson c)
      , ("entries", JsonArray
          [ JsonObject (Map.fromList
              [ ("row", toJson row)
              , ("col", toJson col)
              , ("value", toJson v)
              ])
          | ((row, col), v) <- Map.toList e
          ])
      ])
  fromJson (JsonObject obj) = 
    case (Map.lookup "rows" obj, Map.lookup "cols" obj, Map.lookup "entries" obj) of
      (Just jr, Just jc, Just je) -> case (fromJson jr, fromJson jc, fromJson je) of
        (Just r, Just c, Just es) -> Just (SparseMatrix r c (Map.fromList [((i, j), v) | (i, j, v) <- es :: [(Integer, Integer, a)]]))
        _ -> Nothing
      _ -> Nothing

  fromJson _ = Nothing 

data Tree a = Empty | Tree (Tree a) a (Tree a) deriving (Show, Eq)
instance Jsonable a => Jsonable (Tree a) where
  toJson (Empty) = JsonNull 
  toJson (Tree l v r) =
    JsonObject (Map.fromList [("Left", toJson l), ("Value", toJson v), ("Right", toJson r)]) 
  
  fromJson JsonNull = Just Empty
  fromJson (JsonObject obj) = case (Map.lookup "Left" obj, Map.lookup "Value" obj, Map.lookup "Right" obj) of 
    (Just jl, Just jv, Just jr) -> case (fromJson jl, fromJson jv, fromJson jr) of
      (Just l, Just v, Just r) -> Just (Tree l v r)
      _   -> Nothing
    _     -> Nothing
  fromJson _ = Nothing 

-- Section 3: Num
-- Subsection: Num instances
xor :: Bool -> Bool -> Bool
xor a b = (a || b) && not (a && b)

instance Num Bool where
  (+) = xor
  (*) = (&&)
  (-) a b = a && not b
  abs = id
  signum = id
  fromInteger 0 = False
  fromInteger n = n `mod` 2 == 1
  negate = not

data Expression a
  = Iden String
  | Lit a
  | Plus (Expression a) (Expression a)
  | Minus (Expression a) (Expression a)
  | Mult (Expression a) (Expression a)
  | Div (Expression a) (Expression a)
  | Signum (Expression a)
  deriving (Eq, Show)

instance Num a => Num (Expression a) where
  (+) = Plus
  (-) = Minus
  (*) = Mult
  negate exp = Mult (Lit (-1)) exp
  abs _ = error "abs is not supported for Expression"
  signum exp = Signum exp
  fromInteger n = Lit (fromInteger n)

newtype MatrixSum a = MatrixSum {getMS :: Matrix a} deriving (Show, Eq)

matrixAdd :: Num a => Matrix a -> Matrix a -> Matrix a
matrixAdd (Matrix one) (Matrix two) =
  Matrix (zipWith (zipWith (+)) one two)

newtype MatrixMult a = MatrixMult {getMM :: Matrix a} deriving (Show, Eq)

matrixMul :: Num a => Matrix a -> Matrix a -> Matrix a 
matrixMul (Matrix one) (Matrix two) = 
  Matrix [[sum $ zipWith (*) row col | col <- transpose two] | row <- one] 

instance Num a => Semigroup (MatrixSum a) where
  MatrixSum one <> MatrixSum two = MatrixSum (matrixAdd one two)

instance Num a => Semigroup (MatrixMult a) where
  MatrixMult one <> MatrixMult two = MatrixMult (matrixMul one two)

newtype SparseMatrixSum a = SparseMatrixSum {getSMS :: SparseMatrix a} deriving (Show, Eq)
sparseMatrixAdd :: (Num a, Eq a) => SparseMatrix a -> SparseMatrix a -> SparseMatrix a
sparseMatrixAdd (SparseMatrix r _ one) (SparseMatrix _ c two) =
  let
    summed = Map.unionWith (+) one two
    nonZero = Map.filter (/= 0) summed
  in
    SparseMatrix r c nonZero

newtype SparseMatrixMult a = SparseMatrixMult {getSMM :: SparseMatrix a} deriving (Show, Eq)
sparseMatrixMul :: (Num a, Eq a) => SparseMatrix a -> SparseMatrix a -> SparseMatrix a
sparseMatrixMul (SparseMatrix r _ one) (SparseMatrix _ c two) =
  let 
    products = 
      [ ((i, j), a * b)
      | ((i, k1), a) <- Map.toList one
      , ((k2, j), b) <- Map.toList two
      , k1 == k2
      ]
    summed = Map.fromListWith (+) products
    nonZero = Map.filter (/= 0) summed
  in 
    SparseMatrix r c nonZero

-- These have Eq constraint so you can filter out zero values, which should not appear in sparse matrices.
instance (Num a, Eq a) => Semigroup (SparseMatrixSum a) where 
  SparseMatrixSum one <> SparseMatrixSum two = SparseMatrixSum (sparseMatrixAdd one two)

instance (Num a, Eq a) => Semigroup (SparseMatrixMult a) where 
  SparseMatrixMult one <> SparseMatrixMult two = SparseMatrixMult (sparseMatrixMul one two)

-- Subsection: General functions
evalPoly :: Num a => [a] -> a -> a
evalPoly coeffs x = sum [c * x ^ i | (i, c) <- zip [0 :: Integer ..] coeffs]

type Length = Int
type I = Int
type J = Int
pathsOfLengthK :: Length -> I -> J -> Matrix Int -> Int
pathsOfLengthK k i j m = getVal i j (matrixPower k m)

getVal :: Int -> Int -> Matrix a -> a
getVal i j (Matrix rows) = (rows !! i) !! j

identityMatrix :: Num a => Int -> Matrix a
identityMatrix n = Matrix [[if i == j then 1 else 0 | j <- [0..n-1]] | i <- [0..n-1]] 

matrixPower :: Num a => Int -> Matrix a -> Matrix a
matrixPower 0 (Matrix rows) = identityMatrix (length rows)
matrixPower 1 m = m
matrixPower k m = matrixMul m (matrixPower (k - 1) m)

ourOr :: [Bool] -> Bool
ourOr [] = False
ourOr (x:xs) = x || ourOr xs

hasPath :: I -> J -> Matrix Int -> Bool
hasPath i j m@(Matrix rows) =
  ourOr [ getVal i j (matrixPower k m) > 0 | k <- [1 .. length rows - 1] ]

-- Section 4: Simplify expressions
simplify :: (Expression Integer) -> (Expression Integer)
simplify (Plus e1 e2) = 
  case (simplify e1, simplify e2) of
    (Lit 0, e) -> e
    (e, Lit 0) -> e
    (Lit a, Lit b) -> Lit (a + b)
    (exp1, exp2) -> Plus exp1 exp2
    
simplify (Minus e1 e2) =
  case (simplify e1, simplify e2) of
    (e, Lit 0) -> e
    (Lit a, Lit b) -> Lit (a - b)
    (exp1, exp2) -> Minus exp1 exp2

simplify (Mult e1 e2) =
  case (simplify e1, simplify e2) of
    (Lit 1, e) -> e
    (e, Lit 1) -> e
    (Lit a, Lit b) -> Lit (a * b)
    (exp1, exp2) -> Mult exp1 exp2

simplify (Div e1 e2) =
  case (simplify e1, simplify e2) of
    (e, Lit 1) -> e
    (Lit a, Lit b) -> if b == 0 then Div (Lit a) (Lit b) else Lit (a `div` b)
    (exp1, exp2) -> Div exp1 exp2

simplify (Signum e) =
  case simplify e of
    Lit n -> Lit (signum n)
    Mult a b -> Mult (simplify (Signum a)) (simplify (Signum b))
    Div a b -> Div (simplify (Signum a)) (simplify (Signum b))
    s -> Signum s

simplify (Lit n) = Lit n
simplify (Iden x) = Iden x

inlineExpressions :: [(Expression Integer, String)] -> [(Expression Integer, String)]
inlineExpressions = go [] where
  go _ [] = []
  go env ((e, name):xs) =
    let simplified = simplify e
        alter = changeDups env simplified 
        updatedEnv = env ++ [(alter, name)]
    in (alter, name) : go updatedEnv xs

changeDups :: [(Expression Integer, String)] -> Expression Integer -> Expression Integer
changeDups env expr =
  case searchFor expr env of
    Just name -> Iden name
    Nothing -> case expr of
      Plus a b -> Plus (changeDups env a) (changeDups env b)
      Minus a b -> Minus (changeDups env a) (changeDups env b)
      Mult a b -> Mult (changeDups env a) (changeDups env b)
      Div a b -> Div (changeDups env a) (changeDups env b)
      Signum a -> Signum (changeDups env a)
      _ -> expr
  where
    searchFor _ [] = Nothing
    searchFor e ((prevExpr, name):xs)
      | e == prevExpr = Just name
      | otherwise = searchFor e xs