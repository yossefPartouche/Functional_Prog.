{-# LANGUAGE GHC2024 #-}
-- Implement the following functions.
-- When you're done, ghc -Wall -Werror HW1.hs should successfully compile.
--
-- Tells HLS to show warnings, and the file won't be compiled if there are any warnings, e.g.,
-- eval (-- >>>) won't work.
{-# OPTIONS_GHC -Wall -Werror #-}
-- Refines the above, allowing for unused imports.
{-# OPTIONS_GHC -Wno-unused-imports #-}

module HW1 where

-- These import statement ensures you aren't using any "advanced" functions and types, e.g., lists.
import Prelude (Bool (..), Eq (..), Foldable (sum), Int, Integer, Num (..), Ord (..), abs, div, error, even, flip, fst, id, mod, not, odd, otherwise, snd, take, undefined, ($), (&&), (.), (^), (||))

------------------------------------------------
-- DO NOT MODIFY ANYTHING ABOVE THIS LINE !!! --
------------------------------------------------

---------------------------------------------------
-- Section 1: Function Composition & Transformation
---------------------------------------------------

curry3 :: ((a, b, c) -> d) -> a -> b -> c -> d
curry3 f a b c = f (a, b, c)
uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c
fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a
snd3 :: (a, b, c) -> b
snd3 (_, b, _) = b
thd3 :: (a, b, c) -> c
thd3 (_, _, c) = c
dropFst :: (a, b, c) -> (b, c)
dropFst (_, b, c) = (b, c)
dropSnd :: (a, b, c) -> (a, c)
dropSnd (a, _, c) = (a, c)
dropThd :: (a, b, c) -> (a, b)
dropThd (a, b, _) = (a, b)

mapPair :: (a -> b) -> (a, a) -> (b, b)
mapPair f (x, y) = (f x, f y)
--     (given)v--func1       v--func2 (given)
pairApply :: (a -> b) -> (a -> c) -> a -> (b, c)
pairApply f g x = (f x, g x) 

const :: a -> b -> a
const a _ = a
constSecond :: a -> b -> b
constSecond _ b = b
const2 :: a -> b -> c -> a
const2 a _ _ = a

-- current conclusions:
-- 1) always check the last term in the signature --> indicates what to return
-- 2) The signatures defined over terms "a" ,where "a" may indicate same type but not necessarily same value
-- 3) Think of it like maths, and ask what do you have to work with {function?, term?} 
-- 4) Once you have an idea about 3) ask yourself, how many of each do you have to work with?
-- 5) The above are not enough to get the hang of it

---------------------------
-- Personal: EXTRA PRACTICE
---------------------------
-- Tuple Transformations
swapPair :: (a, b) -> (b, a)
swapPair = undefined

applyTwo :: (a -> b) -> (a -> c) -> a -> (b, c)
applyTwo = undefined

mapTriple :: (a -> b) -> (a, a, a) -> (b, b, b)
mapTriple = undefined

combinePairs :: (a, b) -> (c, d) -> (a, b, c, d)
combinePairs = undefined

getFourth :: (a, b, c, d) -> d
getFourth = undefined

-- Function Combinators
twice :: (a -> a) -> a -> a
twice = undefined

applyN :: Int -> (a -> a) -> a -> a
applyN = undefined

flip3 :: (a -> b -> c -> d) -> c -> b -> a -> d
flip3 = undefined

composeBoth :: (b -> c) -> (a -> b) -> (a -> b) -> a -> (c, c)
composeBoth = undefined

ifThenElse :: (a -> Bool) -> (a -> b) -> (a -> b) -> a -> b
ifThenElse = undefined

-- Working with Higher-Order Functions
applyWhen :: (a -> Bool) -> (a -> a) -> a -> a
applyWhen = undefined

chooseFunction :: (a -> Bool) -> (a -> b) -> (a -> b) -> a -> b
chooseFunction = undefined

selector :: (a -> Int) -> (a -> b) -> (a -> b) -> (a -> b) -> a -> b
selector = undefined

partial :: (a -> b -> c) -> a -> (b -> c)
partial = undefined


-- Generatlizations of (.)
(.) :: (b -> c) -> (a -> b) -> a -> c
(.) f g x = f $ g x
(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d -- Hint: We saw this in class!
(.:) f h x y = f $ h x y
(.:.) :: (d -> e) -> (a -> b -> c -> d) -> a -> b -> c -> e
(.:.) f g x y z = f $ g x y z
(.::) :: (e -> f) -> (a -> b -> c -> d -> e) -> a -> b -> c -> d -> f
(.::) f g w x y z = f $ g w x y z
(.::.) :: (f -> g) -> (a -> b -> c -> d -> e -> f) -> a -> b -> c -> d -> e -> g
(.::.) f g v w x y z = f $ g v w x y z

-- Current Conclusion about reading the signature
-- the brakets in a signature helps us read and deduce the implementation of the function
-- looking inside a perticular bracket we'll always return a single "term"
-- (unless of course the term is a tuple which in some respect is also a single term)
-- we then look at the number elements before the final type and this will tell you
-- how many concrete types there are:
-- Explained Example
-- (.:.) :: (d -> e) -> (a -> b -> c   ->   d) -> a -> b -> c -> e
--             ^         1    2    3   ^  return 
-- READ AS:    f                       g
--                   ^ - could call it a dependant function meaning 
-- it could be a new function term or a composition, depending on the context. 
-- in our case because the returning type from g is what f will use then it's composed
-- In other case we'd deduce something else 


-- How can we ever implement such a function!?
-- we need to consider all possible types at least the main bool, Int, Integer, Double etc
-- exists such a generic function that you can apply with string --> bool and also Int --> Double???
impossible :: a -> b
impossible = undefined
---------------------------------------------------
-- Section 2: Function Composition & Transformation
---------------------------------------------------
-- Count the number of digits of an integer
countDigits :: Integer -> Integer
countDigits = undefined
-- Sums the  digits of an integer
sumDigits :: Integer -> Integer
sumDigits = undefined
-- Reverses the  digits of an integer
reverseDigits :: Integer -> Integer
reverseDigits = undefined
-- Returns the length of the Collatz sequence starting from x. collatzLength 1 = 0. You can assume the input is positive.
collatzLength :: Integer -> Integer
collatzLength = undefined
------------------------
-- Section 3: Generators
------------------------

-- Type definition for a generator: a function producing a sequence of values
-- 1. The first function generates the next value.
-- 2. The second function checks if generation should continue.
-- 3. The third value is the initial value, or seed. It does not count as being generated by the generator.
type Generator a = (a -> a, a -> Bool, a)

-- Retrieves the nth value from a generator. The seed does not count as an element. You can assume n >= 0.
nthGen :: Integer -> Generator a -> a
nthGen = undefined

hasNext :: Generator a -> Bool
hasNext = undefined

-- Behavior is undefined if the generator has stopped.
nextGen :: Generator a -> Generator a
nextGen = undefined

-- Will not terminate if the generator does not stop.
lengthGen :: Generator a -> Integer
lengthGen = undefined

-- Should terminate for infinite generators as well.
hasLengthOfAtLeast :: Integer -> Generator a -> Bool
hasLengthOfAtLeast = undefined

constGen :: a -> Generator a
constGen = undefined

foreverGen :: (a -> a) -> a -> Generator a
foreverGen = undefined

emptyGen :: Generator a
emptyGen = undefined

-- Generates all integers except 0.
integers :: Generator Integer
integers = undefined

-- Sums all the values produced by a generator until it stops.
sumGen :: Generator Integer -> Integer
sumGen = undefined

-- Checks if a generator produces a value that satisfies a predicate.
anyGen :: (a -> Bool) -> Generator a -> Bool
anyGen = undefined

-- Adds an additional predicate to a generator.
andAlso :: (a -> Bool) -> Generator a -> Generator a
andAlso = undefined

-- Bonus (15 points): Generates all positive divisors of a number smaller than the number itself.
divisors :: Integer -> Generator Integer
divisors = undefined

-----------------------------------
-- Section 4: Number classification
-----------------------------------

isPrime :: Integer -> Bool
isPrime = undefined

nextPrime :: Integer -> Integer
nextPrime = undefined

primes :: Generator Integer
primes = undefined

isHappy :: Integer -> Bool
isHappy = undefined

isArmstrong :: Integer -> Bool
isArmstrong = undefined

isPalindromicPrime :: Integer -> Bool
isPalindromicPrime = undefined
