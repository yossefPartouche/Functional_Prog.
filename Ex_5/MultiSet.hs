{-# LANGUAGE GHC2024 #-}
{-# OPTIONS_GHC -Wall -Werror #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module MultiSet (MultiSet, empty, member, count, remove, insert, fromList, toList) where

import Data.Either
import Data.List (find, intercalate, transpose)
import Data.Map qualified as Map
import Data.Maybe
import Data.Semigroup (Arg (..))
import Data.Set qualified as Set
import Prelude (Bool (..), Char, Double, Either (..), Eq (..), Int, Integer, Integral, Maybe (..), Monoid (..), Num (..), Ord (..), Semigroup (..), Show (..), String, all, const, div, drop, error, filter, foldl', foldr, id, init, iterate, length, lookup, map, mod, not, otherwise, product, replicate, reverse, sum, undefined, zip, zipWith, (!!), ($), (&&), (++), (.), (^), (||))

newtype MultiSet a = MultiSet {_getMultiset :: Set.Set (Arg a Int)}

empty :: MultiSet a
empty = MultiSet Set.empty

member :: Ord a => a -> MultiSet a -> Bool
member x (MultiSet ms) = case lookupS x ms of
    Nothing -> False
    Just _  -> True

-- | Returns the count of an element in the multiset, 0 if not present.
count :: Ord a => a -> MultiSet a -> Int
count x (MultiSet ms)  = case lookupS x ms of
    Nothing          -> 0
    Just (Arg _ n)   -> n



-- | Insert one occurrence of an element into the multiset.
insert :: Ord a => a -> MultiSet a -> MultiSet a
insert x (MultiSet ms) = case lookupS x ms of
    Nothing         ->  MultiSet (Set.insert (Arg x 1) ms)
    Just old@(Arg x' n) -> 
        let new = Arg x' (n + 1) 
        in MultiSet $ Set.insert new (Set.delete old ms)

-- | Remove one occurrence of an element from the multiset.
remove :: Ord a => a -> MultiSet a -> MultiSet a
remove x (MultiSet ms) = case lookupS x ms of
    Just (Arg x' 1)     -> MultiSet (Set.delete (Arg x' 1) ms)
    Just old@(Arg x' n) -> 
        let new = Arg x' (n - 1) 
        in MultiSet $ Set.insert new (Set.delete old ms)
    _        ->  MultiSet ms

-- | Convert a list into a multiset.
fromList :: Ord a => [a] -> MultiSet a
fromList = foldr insert empty

-- | Convert a multiset into a list, including duplicates.
toList :: Ord a => MultiSet a -> [a]
toList xs = flattenDup $ Set.toList (_getMultiset xs)

instance Eq a => Eq (MultiSet a) where
    (MultiSet xs) == (MultiSet xs') = xs == xs'
    
{-
-- Might be too much
instance Eq a => Eq (MultiSet a) where
    (MultiSet xs) == (MultiSet xs') = 
        (xs == xs') && (extractCount (Set.toList (xs))) == (extractCount (Set.toList (xs')))
            where 
                extractCount :: [(Arg a Int)] -> [Int]
                extractCount = map (\(Arg _ c)-> c)
-}
instance Show a => Show (MultiSet a) where
    show xs = "{" ++ intercalate "," (map show (flattenDup $ Set.toList (_getMultiset xs))) ++ "}"
instance Ord a => Semigroup (MultiSet a) where
        xs <> xs' = fromList $ toList xs ++ toList xs'
instance Ord a => Monoid (MultiSet a) where
    mempty = empty


---------------- Ex. 5 --------------------- 

instance Foldable MultiSet where
    foldr f z ms = foldOccur (\x cnt acc -> foldr f acc (replicate cnt x)) z ms   

-- | /O(n)/. 
-- Fold over the elements of a multiset with their occurrences.
foldOccur :: (a -> Int -> b -> b) -> b -> MultiSet a -> b
foldOccur _ b (MultiSet ms) | Set.null ms = b
foldOccur f b (MultiSet ms) = 
    Set.foldr (\(Arg x cnt) acc -> f x cnt acc) b ms

