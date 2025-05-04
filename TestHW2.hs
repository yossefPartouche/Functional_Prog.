module TestHW2 where
import Prelude hiding (maybe) -- Hide Prelude's maybe to avoid conflict
import HW2

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