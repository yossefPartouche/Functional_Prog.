module Main where

import HW1

main :: IO ()
main = do
    putStrLn "== Basic Tuple Tests =="
    print $ (curry3 (\(x, y, z) -> x + y + z) 1 2 3 :: Integer) == 6
    print $ (uncurry3 (\x y z -> x + y + z) (1, 2, 3) :: Integer) == 6
    print $ fst3 ((1, 2, 3) :: (Integer, Integer, Integer)) == 1
    print $ snd3 ((1, 2, 3) :: (Integer, Integer, Integer)) == 2
    print $ thd3 ((1, 2, 3) :: (Integer, Integer, Integer)) == 3
    print $ dropFst ((1, 2, 3) :: (Integer, Integer, Integer)) == (2, 3)
    print $ dropSnd ((1, 2, 3) :: (Integer, Integer, Integer)) == (1, 3)
    print $ dropThd ((1, 2, 3) :: (Integer, Integer, Integer)) == (1, 2)

    putStrLn "\n== Digit and Math Tests =="
    print $ countDigits (123456 :: Integer) == 6
    print $ sumDigits (-123 :: Integer) == 6
    print $ reverseDigits (1234 :: Integer) == 4321
    print $ reverseDigits (-1234 :: Integer) == -4321
    print $ collatzLength (3 :: Integer) == 7

    putStrLn "\n== Generator Tests =="
    print $ nthGen 3 positives == (4 :: Integer)
    print $ hasLengthOfAtLeast 5 positives == True
    print $ hasNext emptyGen == False
    print $ take 5 (genToList (constGen (7 :: Integer))) == replicate 5 7

    putStrLn "\n== Prime and Special Number Tests =="
    print $ isPrime (2 :: Integer) == True
    print $ isPrime (9 :: Integer) == False
    print $ isArmstrong (153 :: Integer) == True
    print $ isArmstrong (9474 :: Integer) == True
    print $ isPalindromicPrime (131 :: Integer) == True
    print $ isPalindromicPrime (13 :: Integer) == False

    putStrLn "\n== All tests finished =="

-- Helper: convert a generator to a list safely
genToList :: Generator a -> [a]
genToList (f, c, x)
  | not (c x)  = []
  | otherwise  = x : genToList (f, c, f x)
