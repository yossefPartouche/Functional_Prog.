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
