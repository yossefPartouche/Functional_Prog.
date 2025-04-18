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
import Distribution.Simple.Utils (xargs)
import Distribution.Simple.Program.GHC (GhcOptions(ghcOptThisComponentId))

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

abs' :: Integer -> Integer
abs' x
    | x >= 0 = x
    | otherwise =  -x

isNeg :: Integer -> Bool
isNeg x = if x < 0 then True else False

countDigits :: Integer -> Integer
countDigits x
    | x < 0 = countDigits $ abs' x
    | x < 10 = 1
    | otherwise = 1 + (countDigits $ div x 10)
-- functions in haskell are noted by space i.e. spacing is important
-- the brackets are used for grouping 
-- the recursive is the same as 1 + countDigits (div x 10) or 
-- 1 + countDigits (x `div` 10)

-- Sums the  digits of an integer
sumDigits :: Integer -> Integer
sumDigits x
    | x < 0 = sumDigits $ abs' x
    | x < 10 = x
    | otherwise = x `mod` 10 + sumDigits (x `div` 10)

-- Reverses the  digits of an integer
reverseDigits :: Integer -> Integer
reverseDigits x = 
    let isNegative = isNeg x
        absX  = abs' x
        reverse = helpReverse absX 0
    in if isNegative then - reverse else reverse
    where 
        helpReverse 0 acc = acc
        helpReverse n acc = helpReverse (n `div` 10) (acc * 10 + n `mod` 10)

-- might want to the countDigits method as well

reverseDigits' :: Integer -> Integer
reverseDigits' x = 
    let isNegative' = isNeg x
        absX' = abs' x
        length = countDigits x
        reverse' = helpReverse' absX' length
    in if isNegative' then - reverse' else reverse'
    where
        helpReverse' n 1 = n
        helpReverse' n len = helpReverse' (n `div` 10)  (len -1) + n `mod` 10 * 10 ^ (len -1)

        
-- Returns the length of the Collatz sequence starting from x. collatzLength 1 = 0. You can assume the input is positive.
collatzLength :: Integer -> Integer
collatzLength n = 
    if n == 1 then 0
    else if n `mod` 2 == 0 then 1 + collatzLength (n `div` 2)
    else 1 + collatzLength (3 * n +1)
------------------------
-- Section 3: Generators
------------------------

-- Type definition for a generator: a function producing a sequence of values
-- 1. The first function generates the next value.
-- 2. The second function checks if generation should continue.
-- 3. The third value is the initial value, or seed. It does not count as being generated by the generator.
type Generator a = (a -> a, a -> Bool, a)

positives :: Generator Integer
positives = ((+ 1), const True, 0)

-- Retrieves the nth value from a generator. The seed does not count as an element. You can assume n >= 0.
nthGen :: Integer -> Generator a -> a
nthGen n (increment, continue, indx) =
    if n <= 0 then increment indx
    else nthGen (n - 1) (increment, continue, increment indx)

hasNext :: Generator a -> Bool
hasNext (_, check, value) = 
    if check value then True else False
--hasNext (doSomething , check, value) = 
    --if check value then True else False

-- Behavior is undefined if the generator has stopped.
nextGen :: Generator a -> Generator a
nextGen (takeMeSomewhere, canIgo, toPlace) = 
    if not (canIgo toPlace) then undefined 
    else (takeMeSomewhere, canIgo, takeMeSomewhere toPlace)

-- Will not terminate if the generator does not stop.
lengthGen :: Generator a -> Integer
lengthGen (somefun, check, val) = 
    if not (check val) then 0
    else delayCount (somefun val)
    where 
        delayCount v =
            if not (check v) then 1 
            else 1 + delayCount(somefun v)


-- Should terminate for infinite generators as well.
hasLengthOfAtLeast :: Integer -> Generator a -> Bool
hasLengthOfAtLeast n (somefun, check, val) = 
    if n == 0 then True
    else if hasNext (somefun, check, val) then hasLengthOfAtLeast (n - 1) (somefun, check, somefun val)
    else False

constGen :: a -> Generator a
constGen x = (\_ -> x, const True, x)

foreverGen :: (a -> a) -> a -> Generator a
foreverGen f x = (f, \_ -> True, x)

emptyGen :: Generator a
emptyGen = (id,const False, undefined)

-- Generates all integers except 0.
integers :: Generator Integer
integers = (altFun, const True, 0)
    where 
    altFun n
        | n > 0 = - n
        | otherwise = abs' n + 1

-- Sums all the values produced by a generator until it stops.
sumGen :: Generator Integer -> Integer
sumGen (fun, check, val) = 
    if not $ check val then 0 else
    startSumming (fun val)
    where 
        startSumming v = 
            if not $ check v then 0 else v + startSumming (fun v)


-- Checks if a generator produces a value that satisfies a predicate.
-- This is a predicate function exists a case such ... 
-- if returns false --> for every case it's not satisfied
anyGen :: (a -> Bool) -> Generator a -> Bool
anyGen pred (fun, check, val) =
    if not $ check val then False
    else pred (fun val) ||  anyGen pred (fun, check, fun val)

-- Adds an additional predicate to a generator.
andAlso :: (a -> Bool) -> Generator a -> Generator a
andAlso addPred (func, oldPred, a) = (func, \x -> addPred x && oldPred x, a)

-- Bonus (15 points): Generates all positive divisors of a number smaller than the number itself.
divisors :: Integer -> Generator Integer
divisors n = (func, (< n), 1)
    where 
        func a = 
            let curr = a + 1
            in if n `mod` curr == 0 then curr else func curr

-----------------------------------
-- Section 4: Number classification
-----------------------------------

isPrime :: Integer -> Bool
isPrime n
    | n <= 1    = False
    | otherwise = nthGen 0 (divisors n) == n

nextPrime :: Integer -> Integer
nextPrime n = 
    let a = n + 1
    in if isPrime a then a else nextPrime a
    
primes :: Generator Integer
primes = (nextPrime, const True, 2)

-- Sums the digits to a specified power of an integer (helper function for isHappy and isArmstrong)
sumDigitsToPower :: Integer -> Integer -> Integer
sumDigitsToPower x pow 
    | x < 0     = sumDigitsToPower (abs' x) pow
    | x < 10    = x ^ pow
    | otherwise = (x `mod` 10) ^ pow + sumDigitsToPower (x `div` 10) pow

isHappy :: Integer -> Bool
isHappy n = findCycle n (sumDigitsToPower n 2)
  where
    findCycle slow fast
      | slow == 1 = True
      | fast == 1 = True
      | slow == fast = False  
      | otherwise = findCycle (sumDigitsToPower slow 2) (sumDigitsToPower (sumDigitsToPower fast 2) 2)

isArmstrong :: Integer -> Bool
isArmstrong n
    | n < 0     = False
    | otherwise = sumDigitsToPower n (countDigits n) == n

isPalindromicPrime :: Integer -> Bool
isPalindromicPrime n = 
    isPrime n && reverseDigits n == n
