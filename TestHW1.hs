module Main where

import HW1

main :: IO ()
main = do
    putStrLn "== Additional Tests from PDF =="

    -- countDigits
    print $ countDigits (0 :: Integer) == 1
    print $ countDigits (1024 :: Integer) == 4
    print $ countDigits (-42 :: Integer) == 2

    -- sumDigits
    print $ sumDigits (0 :: Integer) == 0
    print $ sumDigits (1024 :: Integer) == 7
    print $ sumDigits (-42 :: Integer) == 6

    -- reverseDigits
    print $ reverseDigits (120 :: Integer) == 21
    print $ reverseDigits (-42 :: Integer) == -24

    -- collatzLength
    print $ collatzLength (1 :: Integer) == 0
    print $ collatzLength (2 :: Integer) == 1
    print $ collatzLength (4 :: Integer) == 2
    print $ collatzLength (1024 :: Integer) == 10
    print $ collatzLength (1025 :: Integer) == 36

    -- nthGen edge cases
    print $ nthGen (-1) (positives :: Generator Integer) == 0
    print $ nthGen 42 (((+1), (< 10), 0) :: Generator Integer) == 10

    -- hasNext
    print $ hasNext (((+1), (<= 0), 0) :: Generator Integer) == True
    print $ hasNext (((+1), (<= 0), 1) :: Generator Integer) == False

    -- nextGen
    --print $ thd3 (nextGen (((+1), (< 0), 0) :: Generator Integer)) == 1

    -- lengthGen
    print $ lengthGen (((+1), (< 0), 0) :: Generator Integer) == 0
    print $ lengthGen (((+1), (<= 0), 0) :: Generator Integer) == 1
    print $ lengthGen (((+1), (< 10), 0) :: Generator Integer) == 10

    -- hasLengthOfAtLeast
    print $ hasLengthOfAtLeast 10 (((+1), (< 10), 0) :: Generator Integer) == True
    print $ hasLengthOfAtLeast 10 (((+1), (< 9), 0) :: Generator Integer) == False
    print $ hasLengthOfAtLeast 42 (positives :: Generator Integer) == True

    -- constGen and emptyGen
    print $ nthGen 42 (constGen ("foobar" :: String)) == "foobar"
    print $ lengthGen (emptyGen :: Generator Int) == 0
    print $ lengthGen (emptyGen :: Generator (Int, Int)) == 0
    print $ lengthGen (emptyGen :: Generator (Generator Int)) == 0

    -- anyGen
    print $ anyGen (== 42) (integers :: Generator Integer) == True
    print $ anyGen (== (-42)) (integers :: Generator Integer) == True
    -- Skipping: anyGen (== 0) integers — would not terminate

    -- sumGen
    print $ sumGen (((+1), (<= 1), 0) :: Generator Integer) == 3
    print $ sumGen (((+1), (<= 1), 1) :: Generator Integer) == 2
    print $ sumGen (((+1), (< 10), 0) :: Generator Integer) == 55
    print $ sumGen (((+1), (< 10), 1) :: Generator Integer) == 54
    print $ sumGen (emptyGen :: Generator Integer) == 0

    -- andAlso
    print $ lengthGen (andAlso (< 10) (positives :: Generator Integer)) == 10
    print $ lengthGen (andAlso (> 20) (andAlso (< 10) (positives :: Generator Integer))) == 0
    print $ lengthGen (andAlso (< 20) (andAlso (< 10) (positives :: Generator Integer))) == 10

    -- nextPrime
    print $ nextPrime (-42) == 2
    print $ nextPrime 1 == 2
    print $ nextPrime 2 == 3
    print $ nextPrime 100 == 101

    -- nthGen on primes
    print $ nthGen 0 (primes :: Generator Integer) == 2
    print $ nthGen 100 (primes :: Generator Integer) == 547

    -- isHappy
    print $ isHappy (7 :: Integer) == True
    print $ isHappy (42 :: Integer) == False
    print $ isHappy (130 :: Integer) == True
    print $ isHappy (-130 :: Integer) == True

    -- isArmstrong
    print $ isArmstrong (0 :: Integer) == True
    print $ isArmstrong (1 :: Integer) == True
    print $ isArmstrong (42 :: Integer) == False
    print $ isArmstrong (153 :: Integer) == True

    -- isPalindromicPrime
    print $ isPalindromicPrime (2 :: Integer) == True
    print $ isPalindromicPrime (11 :: Integer) == True
    print $ isPalindromicPrime (13 :: Integer) == False
    print $ isPalindromicPrime (101 :: Integer) == True

    putStrLn "\n== PDF-based tests completed =="
