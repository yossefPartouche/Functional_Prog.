module TestHW2 where
import Prelude hiding (maybe, zip, zipWith, unzip, drop, takeWhile, dropWhile) -- Hide Prelude's maybe to avoid conflict
import HW2 hiding (take)

main :: IO ()
main = do
        -- Section 1.1 Basic Maybe tests
        print $ fromMaybe 1 Nothing == 1
        print $ fromMaybe undefined (Just 2) == 2
        print $ maybe 1 undefined Nothing == 1
        print $ maybe undefined length (Just "foo") == 3
        print $ (maybeHead [1,2,3] :: Maybe Int) == Just 1
        print $ (maybeHead [] :: Maybe Int) == Nothing
        print $ (maybeLast [1,2,3] :: Maybe Int) == Just 3
        print $ (maybeLast [] :: Maybe Int) == Nothing
        print $ maybeMaximum [1,2,3] == Just 3
        print $ maybeMaximum [] == Nothing
        print $ maybeMinimum [1,2,3] == Just 1
        print $ maybeMinimum [] == Nothing
        print $ filterMaybe even (Just 2) == Just 2
        print $ filterMaybe even (Just 3) == Nothing
        print $ sumMaybe (Just 1) (Just 2) == Just 3
        print $ sumMaybe (Just 1) Nothing == Nothing
        print $ liftMaybe2 (*) (Just 2) (Just 3) == Just 6
        print $ (liftMaybe2 (+) Nothing (Just 3) :: Maybe Int) == Nothing
        print $ mapMaybe (\e -> if even e then Just (e * 2) else Nothing) [1,2,3] == [4]
        print $ catMaybes [Just 1, Just 2, Nothing, Just 3] == [1,2,3]
        putStrLn "✅ Done testing Maybe utilities."

        -- Section 1.2 Either functions tests
        print $ (fromEither undefined (Right 1) :: Integer) == 1
        print $ (fromEither "foo" (Left 42) :: String) == "foo"
        print $ (catEithers [Right 10, Right 20] :: Either String [Integer]) == Right [10, 20]
        print $ (catEithers [Right 10, Left "foo", Right 20, Left "bar"] :: Either String [Integer]) == Left "foo"
        print $ (liftEither2 (*) (Right 2) (Right 3) :: Either String Integer) == Right 6
        print $ (mapEither (\x -> if x > 0 then Right $ x * 10 else Left $ x + 5) [1, 2, 3] :: Either Integer [Integer]) == Right [10,20,30]
        print $ (mapEither (\x -> if x > 0 then Right $ x * 10 else Left $ x + 5) [1, -1, 2, -2] :: Either Integer [Integer]) == Left 4
        print $ partitionEithers [Right "foo", Left 42, Left 54, Right "bar"] == ([42,54],["foo","bar"])
        print $ (liftEither2 (*) (Right 2) (Right 3) :: Either String Integer) == Right 6
        print $ (liftEither2 undefined (Right 2) (Left "foo") :: Either String Integer) == Left "foo"
        putStrLn "✅ Done testing Either utilities."

        -- Section 2: Expression Tests (from images)
        putStrLn "\nTesting Expression Functions:"

        -- Test exprToString (from Image 1)
        putStrLn "\nTesting exprToString:"
        print $ exprToString (Lit 1 `Plus` Lit 2) == "1 + 2"
        print $ exprToString (Iden "x" `Plus` (Lit 2 `Div` Lit 3)) == "x + (2 / 3)"

        -- Tests for exprToString' (bonus implementation)
        putStrLn "\nTesting exprToString' (minimal parentheses):"
        print $ exprToString' (Lit 1 `Plus` Lit 2) == "1 + 2"
        print $ exprToString' ((Iden "x" `Plus` Lit 42 `Plus` Iden "y") `Mul` Lit 2 `Plus` Lit 3) == "(x + 42 + y) * 2 + 3"
        print $ exprToString' (Lit 1 `Minus` (Lit 2 `Minus` Lit 3)) == "1 - (2 - 3)"
        print $ exprToString' ((Lit 1 `Minus` Lit 2) `Minus` Lit 3) == "1 - 2 - 3"
        print $ exprToString' (Lit 1 `Plus` (Lit 2 `Mul` Lit 3)) == "1 + 2 * 3"
        print $ exprToString' ((Lit 1 `Plus` Lit 2) `Mul` Lit 3) == "(1 + 2) * 3"
        print $ exprToString' (Lit 1 `Mul` Lit 2 `Plus` Lit 3) == "1 * 2 + 3"
        print $ exprToString' (Lit 1 `Mul` (Lit 2 `Plus` Lit 3)) == "1 * (2 + 3)"
        print $ exprToString' (Lit 1 `Div` Lit 2 `Div` Lit 3) == "1 / 2 / 3"
        print $ exprToString' (Lit 1 `Div` (Lit 2 `Div` Lit 3)) == "1 / (2 / 3)"
        print $ exprToString' ((Lit 1 `Div` Lit 2) `Mul` Lit 3) == "1 / 2 * 3"
        print $ exprToString' (Lit 1 `Div` (Lit 2 `Mul` Lit 3)) == "1 / (2 * 3)"
        print $ exprToString' (Iden "x" `Plus` Iden "y" `Plus` Iden "z") == "x + y + z"
        print $ exprToString' (Iden "x" `Mul` Iden "y" `Mul` Iden "z") == "x * y * z"
        print $ exprToString' ((Lit 1 `Plus` Lit 2) `Div` (Lit 3 `Minus` Lit 4)) == "(1 + 2) / (3 - 4)"

        putStrLn "✅ Done testing exprToString'."

        -- Test partialEvaluate (from Image 2)
        putStrLn "\nTesting partialEvaluate with environments:"
        print $ partialEvaluate [("x", 42)] ((Lit 1 `Plus` Iden "x") `Mul` (Iden "y" `Minus` Lit 2)) 
                == Just (Mul (Lit 43) (Minus (Iden "y") (Lit 2)))
        print $ partialEvaluate [("x", 42)] ((Lit 1 `Plus` Iden "x") `Mul` (Iden "y" `Div` Lit 0)) 
                == Nothing
        

        -- More partialEvaluate tests (from Image 3)
        putStrLn "\nTesting more partialEvaluate cases:"
        print $ partialEvaluate [] (negateExpr (Lit 20 `Minus` Lit 62)) == Just (Lit 42)
        print $ partialEvaluate [] (Lit 42 `powerExpr` 0) == Just (Lit 1)
        print $ partialEvaluate [] (Lit 0 `powerExpr` 0) == Just (Lit 1)  -- 0^0 = 1
        print $ partialEvaluate [("x", 2)] ((Iden "x" `Plus` Lit 4) `powerExpr` 3) == Just (Lit 216)
        print $ partialEvaluate [] ((Lit 2 `Plus` Lit 3) `powerExpr` (-1)) == Just (Lit 0)  -- n^(-m) = 0 for n < 0
        print $ partialEvaluate [] ((Lit 2 `Div` Lit 0) `powerExpr` (-1)) == Nothing  -- Division by 0 is bad
        print $ partialEvaluate [] ((Lit 2 `Div` Lit 0) `powerExpr` 0) == Nothing  -- Division by 0 is still bad
        print $ partialEvaluate [("x", 2), ("y", 3)] (Lit 42 `modExpr` (Iden "x" `Plus` Iden "y")) == Just (Lit 2)  -- 42 % 5 = 2
        print $ partialEvaluate [] (Lit 42 `modExpr` Lit 0) == Nothing  -- Mod by 0 is bad

        putStrLn "✅ Done testing Expression functions."
        -- Tests for zip
        putStrLn "\nTesting zip functions:"
        print $ zip [1, 2, 3] ['a', 'b', 'c'] == [(1,'a'), (2,'b'), (3,'c')]
        print $ zip [1, 2] ['a', 'b', 'c', 'd'] == [(1,'a'), (2,'b')]
        print $ zip [1, 2, 3, 4] ['a', 'b'] == [(1,'a'), (2,'b')]
        --print $ zip [] [1, 2, 3] == []
        print $ take 3 (zip [1..] ['a', 'b', 'c']) == [(1,'a'), (2,'b'), (3,'c')]

        -- Tests for zipWithIndex
        putStrLn "\nTesting zipWithIndex:"
        print $ zipWithIndex ['a', 'b', 'c'] == [(1,'a'), (2,'b'), (3,'c')]
        --print $ zipWithIndex [] == []
        print $ zipWithIndex [42] == [(1,42)]
        print $ take 5 (zipWithIndex [10..]) == [(1,10), (2,11), (3,12), (4,13), (5,14)]

        -- Tests for zipWith
        putStrLn "\nTesting zipWith:"
        print $ zipWith (+) [1, 2, 3] [4, 5, 6] == [5, 7, 9]
        print $ zipWith (*) [1, 2, 3] [4, 5, 6] == [4, 10, 18]
        print $ zipWith (\x y -> x ++ " " ++ y) ["hello", "good"] ["world", "bye"] == ["hello world", "good bye"]
        print $ zipWith (+) [] [1, 2, 3] == []
        print $ take 3 (zipWith (+) [1..] [10, 20, 30]) == [11, 22, 33]

        -- Tests for zipWithDefault
        putStrLn "\nTesting zipWithDefault:"
        print $ (zipWithDefault 0 'a' ([1,2,3] :: [Int]) "foobar") == [(1,'f'),(2,'o'),(3,'o'),(0,'b'),(0,'a'),(0,'r')]
        print $ (zipWithDefault 0 'a' ([1..6] :: [Int]) "foo") == [(1,'f'),(2,'o'),(3,'o'),(4,'a'),(5,'a'),(6,'a')]
        print $ (take 10 $ zipWithDefault 1 'a' ([1..] :: [Int]) "foo") == [(1,'f'),(2,'o'),(3,'o'),(4,'a'),(5,'a'),(6,'a'),(7,'a'),(8,'a'),(9,'a'),(10,'a')]

        -- Tests for zipEither
        putStrLn "\nTesting zipEither:"
        print $ (zipEither ([1, 2] :: [Int]) "foobar") == Left ErrorFirst
        print $ (zipEither ([1..] :: [Int]) "foobar") == Left ErrorSecond
        print $ (zipEither ([1, 2, 3] :: [Int]) "foo") == Right [(1,'f'),(2,'o'),(3,'o')]
        -- Tests for unzip
        putStrLn "\nTesting unzip:"
        print $ unzip [(1,'a'), (2,'b'), (3,'c')] == ([1,2,3], ['a','b','c'])
        print $ (unzip ([] :: [(Int, Char)])) == (([] :: [Int]), ([] :: [Char]))
        print $ unzip [(True,"hello"), (False,"world")] == ([True,False], ["hello","world"])

        -- Tests for unzipFirst
        putStrLn "\nTesting unzipFirst:"
        print $ unzipFirst [(1,'a'), (2,'b'), (3,'c')] == [1,2,3]
        print $ (unzipFirst ([] :: [(Int, Char)])) == ([] :: [Int])
        print $ unzipFirst [(10,"ten"), (20,"twenty"), (30,"thirty")] == [10,20,30]
        
        -- Tests for unzipSecond
        putStrLn "\nTesting unzipSecond:"
        print $ unzipSecond [(1,'a'), (2,'b'), (3,'c')] == ['a','b','c']
        print $ (unzipSecond ([] :: [(Int, Char)])) == ([] :: [Char])
        print $ unzipSecond [(10,"ten"), (20,"twenty"), (30,"thirty")] == ["ten","twenty","thirty"]

        putStrLn "✅ Done testing Zip functions."

        -- Tests for take
        putStrLn "\nTesting take:"
        print $ take 2 [1, 2, 3] == [1, 2]
        print $ take 4 ([1..] :: [Int]) == [1, 2, 3, 4]
        print $ take 0 [1, 2, 3] == []
        print $ take (-1) [1, 2, 3] == []
        print $ (take 5 ([] :: [Int])) == ([] :: [Int])

        -- Tests for drop
        putStrLn "\nTesting drop:"
        print $ drop 2 [1, 2, 3] == [3]
        print $ drop 0 [1, 2, 3] == [1, 2, 3]
        print $ drop 5 [1, 2, 3] == []
        print $ drop (-1) [1, 2, 3] == [1, 2, 3]
        print $ (drop 2 ([] :: [Int]) == ([] :: [Int]))

        -- Tests for slice
        putStrLn "\nTesting slice:"
        print $ slice 2 5 ([1..] :: [Int]) == [3, 4, 5]
        print $ slice 0 3 [1, 2, 3, 4, 5] == [1, 2, 3]
        print $ slice 3 3 [1, 2, 3, 4, 5] == [4]
        print $ slice 5 2 [1, 2, 3, 4, 5] == []
        print $ slice 10 15 ([1..] :: [Int]) == [11, 12, 13, 14, 15]

        -- Tests for snoc
        putStrLn "\nTesting snoc:"
        print $ snoc [2, 3] 1 == [2, 3, 1]
        print $ take 5 (snoc ([1..] :: [Int]) 0) == [1, 2, 3, 4, 5]
        print $ snoc [] 1 == [1]
        print $ snoc (snoc [1, 2] 3) 4 == [1, 2, 3, 4]
        print $ snoc [True, False] True == [True, False, True]

        -- Tests for takeWhile
        putStrLn "\nTesting takeWhile:"
        print $ takeWhile (< 5) ([1..] :: [Int]) == [1, 2, 3, 4]
        print $ takeWhile even [2, 4, 6, 7, 8, 10] == [2, 4, 6]
        print $ takeWhile odd [2, 4, 6] == []
        print $ takeWhile (> 0) [] == []
        print $ take 3 (takeWhile (> 0) ([1..] :: [Int])) == [1, 2, 3]

        -- Tests for dropWhile
        putStrLn "\nTesting dropWhile:"
        print $ take 5 (dropWhile (< 5) ([1..] :: [Int])) == [5, 6, 7, 8, 9]
        print $ dropWhile even [2, 4, 6, 7, 8, 10] == [7, 8, 10]
        print $ dropWhile odd [2, 4, 6] == [2, 4, 6]
        print $ dropWhile (< 0) [] == []
        print $ dropWhile (< 100) [1, 2, 3] == []

        -- Tests for takeEvery
        putStrLn "\nTesting takeEvery:"
        print $ take 5 (takeEvery 3 ([1..] :: [Int])) == [3, 6, 9, 12, 15]
        print $ takeEvery 2 [1, 2, 3, 4, 5] == [2, 4]
        print $ takeEvery 4 [1, 2, 3] == []
        print $ takeEvery 0 [1, 2, 3] == []
        print $ takeEvery (-1) [1, 2, 3] == []

        -- Tests for dropEvery
        putStrLn "\nTesting dropEvery:"
        print $ take 5 (dropEvery 3 ([1..] :: [Int])) == [1, 2, 4, 5, 7]
        print $ dropEvery 2 [1, 2, 3, 4, 5] == [1, 3, 5]
        print $ dropEvery 1 [1, 2, 3] == []
        print $ dropEvery 0 [1, 2, 3] == [1, 2, 3]
        print $ dropEvery 10 [1, 2, 3] == [1, 2, 3]


        -- Tests for nub
        putStrLn "\nTesting nub:"
        print $ nub [1, 1, 2, 3, 2, 4] == [1, 2, 3, 2, 4]
        print $ take 5 (nub ([1..] :: [Int])) == [1, 2, 3, 4, 5]
        print $ nub [] == []
        print $ nub [5, 5, 5, 5] == [5]
        print $ nub [1, 2, 2, 3, 1, 2, 3, 4] == [1, 2, 3, 1, 2, 3, 4]

        -- Tests for infiniteRepeats with nub
        putStrLn "\nTesting infiniteRepeats with nub:"
        let infiniteRepeats = [tripleX | x <- [1..], tripleX <- [x, x, x]]
        print $ take 15 infiniteRepeats == [1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4, 5, 5, 5]
        print $ take 15 (nub infiniteRepeats) == [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15]

        -- Tests for uniq
        putStrLn "\nTesting uniq:"
        print $ uniq [1, 1, 2, 3, 2, 4] == [1, 2, 3, 4]
        print $ uniq [] == []
        print $ uniq [5, 5, 5, 5] == [5]
        print $ take 5 (uniq $ concat [[x, x] | x <- [1..]]) == [1, 2, 3, 4, 5]
        print $ uniq [3, 1, 2, 1, 2, 3] == [3, 1, 2]

        -- Tests for cartesianWith
        putStrLn "\nTesting cartesianWith:"
        print $ (cartesianWith (*) ([1, 2] :: [Int]) ([3, 4] :: [Int])) == [3, 4, 6, 8]
        print $ (cartesianWith (,) ([1, 2] :: [Int]) (['a', 'b'] :: [Char])) == [(1,'a'), (1,'b'), (2,'a'), (2,'b')]
        print $ cartesianWith (++) ["hello", "hi"] [" world", " there"] == ["hello world", "hello there", "hi world", "hi there"]
        print $ cartesianWith (+) [] ([1, 2] :: [Int]) == []
        print $ cartesianWith (+) ([1, 2] :: [Int]) [] == []
        print $ length (take 9 (cartesianWith (*) ([1..3] :: [Int]) ([1..3] :: [Int]))) == 6

        -- Tests for toBase64
        putStrLn "\nTesting toBase64:"
        print $ toBase64 0 == "A"
        print $ toBase64 1 == "B"
        print $ toBase64 25 == "Z"
        print $ toBase64 26 == "a"
        print $ toBase64 63 == "/"
        print $ toBase64 64 == "BA"
        print $ toBase64 509701384549 == "Haskell"
        print $ toBase64 (-509701384549) == "-Haskell"
        print $ toBase64 (-1) == "-B"

        -- Tests for fromBase64
        putStrLn "\nTesting fromBase64:"
        print $ fromBase64 "A" == Just 0
        print $ fromBase64 "B" == Just 1
        print $ fromBase64 "Z" == Just 25
        print $ fromBase64 "a" == Just 26
        print $ fromBase64 "/" == Just 63
        print $ fromBase64 "BA" == Just 64
        print $ fromBase64 "Haskell" == Just 509701384549
        print $ fromBase64 "-Haskell" == Just (-509701384549)
        print $ fromBase64 "-B" == Just (-1)
        print $ fromBase64 "--B" == Just 1  -- Double negative becomes positive
        print $ fromBase64 "Haskell?" == Nothing  -- Invalid character
        print $ fromBase64 "" == Nothing  -- Empty string
        print $ fromBase64 "-" == Nothing  -- Just a minus sign

        -- Round-trip tests
        putStrLn "\nTesting round-trip conversions:"
        print $ (fromBase64 (toBase64 42) == Just 42)
        print $ (fromBase64 (toBase64 (-42)) == Just (-42))
        print $ (fromBase64 (toBase64 509701384549) == Just 509701384549)
        print $ (fromBase64 (toBase64 0) == Just 0)