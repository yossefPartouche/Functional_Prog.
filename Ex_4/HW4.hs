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
  toJson s             = JString s

  fromJson (JString s) = Just s
  fromJson _           = Nothing

instance Jsonable Integer where
  toJson v             = JsonInt s

  fromJson (JsonInt v) = Just v
  fromJson _           = Nothing

instance Jsonable Double where 
  toJson d                = JsonDouble d

  fromJson (JsonDouble d) = Just d
  fromJson _              = Nothing 

instance (Jsonable a, Jsonable b) => Jsonable (a, b) where 
  toJson (a,b) = JsonArray [toJson a, toJson b]

  fromJson (JsonArray [a,b]) = case (fromJson a, fromJson b) of
    (Just a, Just b) -> Just (a,b)
    _                -> Nothing

instance (Jsonable a, Jsonable b, Jsonable c) => Jsonable (a, b, c) where
  toJson (a,b,c) = JsonArray [toJson a, toJson b, toJson c]

  fromJson (JsonArray [a,b,c]) = case (fromJson a, fromJson b, fromJson c) of
    (Just a, Just b, Just c) -> Just (a,b,c)
    _                -> Nothing

--Hard
instance Jsonable a => Jsonable (Maybe a) where 

  toJson Nothing = JsonNull
  toJson (Just x)  = JsonObject (Map.singleton "Just" (toJson x))

  -- JsonNull is legitimate of Json it happens to be Nothing, so it's Just Nothing
  fromJson JsonNull = Just Nothing 
  fromJson (JsonObject k) = case Map.lookup "Just" k of 
    -- Jsonable object that isn't Null so we return Just that
    Just v  -> Just fmap Just (fromJson v) 
    -- We didn't find a map
    Nothing -> Nothing 
  fromJson _ = Nothing 

-- Harder 
instance (Jsonable l, Jsonable r) => Jsonable (Either l r) where 
  toJson (Left x) = JsonObject (Map.singleton "Left" (toJson x))
  toJson (Right y) = JsonObject (Map.singleton "Right" (toJson y))

  fromJson (JsonObject k) = case Map.lookup "Left" k of
    Just v -> Map.fmap Left (fromJson v)
    Nothing -> 
      case Map.lookup "Right" k of
        Just v' -> fmap Right (fromJson v')
        Nothing -> Nothing

instance Jsonable a => Jsonable [a] where
  {-
    the input will be a list, we want to return it to a JsonList
    map does this for us 
    just like map (*2) [1,2,3] --> [2,4,6]
    map toJson [Jsonable1, Jsonabl2, Jsonable3] --> [Jsoned1, Jsoned2, Jsoned3]
    and all this is Jsonable therefore JsonArray (map toJson [Jsonable1, Jsonabl2, Jsonable3])
    --> JsonedArray [Jsoned1, Jsoned2, Jsoned3]
  -} 
  toJson xs = JsonArray (map toJson xs)

  fromJson (JsonArray xs) = (fmap fromJson xs)

data Matrix a = Matrix [[a]] deriving (Show, Eq)

instance (Jsonable a, Ord a) => Jsonable (MS.MultiSet a)
instance Jsonable a => Jsonable (Matrix a)

-- A sparse matrix is a more efficient representation of a matrix when most of the entries are zero.
-- Note that zero values should not appear in the map.
data SparseMatrix a
  = SparseMatrix
  { rows :: Integer
  , cols :: Integer
  , entries :: Map.Map (Integer, Integer) a
  }
  deriving (Show, Eq)
instance Jsonable a => Jsonable (SparseMatrix a)

data Tree a = Empty | Tree (Tree a) a (Tree a) deriving (Show, Eq)
instance Jsonable a => Jsonable (Tree a)

-- Section 3: Num
-- Subsection: Num instances
instance Num Bool

data Expression
  = Iden String
  | Lit Integer
  | Plus Expression Expression
  | Minus Expression Expression
  | Mult Expression Expression
  | Div Expression Expression
  | Signum Expression
  deriving (Eq, Show)
instance Num Expression

newtype MatrixSum a = MatrixSum {getMS :: Matrix a} deriving (Show, Eq)
newtype MatrixMult a = MatrixMult {getMM :: Matrix a} deriving (Show, Eq)
instance Num a => Semigroup (MatrixSum a)
instance Num a => Semigroup (MatrixMult a)

newtype SparseMatrixSum a = SparseMatrixSum {getSMS :: SparseMatrix a} deriving (Show, Eq)
newtype SparseMatrixMult a = SparseMatrixMult {getSMM :: SparseMatrix a} deriving (Show, Eq)

-- These have Eq constraint so you can filter out zero values, which should not appear in sparse matrices.
instance (Num a, Eq a) => Semigroup (SparseMatrixSum a)
instance (Num a, Eq a) => Semigroup (SparseMatrixMult a)

-- Subsection: General functions
evalPoly :: Num a => [a] -> Integer -> a

type Length = Int
type I = Int
type J = Int
pathsOfLengthK :: Length -> I -> J -> Matrix Int -> Int
hasPath :: I -> J -> Matrix Int -> Bool
-- Section 4: Simplify expressions
simplify :: Expression -> Expression
inlineExpressions :: [(Expression, String)] -> [(Expression, String)]
