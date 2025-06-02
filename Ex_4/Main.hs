import Test.HUnit
import Data.Map (fromList)
import Data.List (transpose)
import HW4
import MultiSet qualified as MS


-- Assume all relevant data types and functions have been imported:
-- Matrix, SparseMatrix, Expression, MatrixSum, MatrixMult, SparseMatrixSum, SparseMatrixMult, etc.

-- Basic tests from the first image
basicTests :: Test
basicTests = TestList
  [ "Bool + Bool" ~: (False :: Bool) ~=? (True + True)
  , "Bool - False" ~: (True :: Bool) ~=? (True - False)
  , "9 :: Bool" ~: True ~=? (9 :: Bool)
  , "42 :: Bool" ~: False ~=? (42 :: Bool)
  ]

-- MatrixSum tests
matrixSumTests :: Test
matrixSumTests = TestList
  [ "MatrixSum addition (Ints)" ~:
      MatrixSum (Matrix [[2,4],[6,8]]) ~=? 
      (MatrixSum (Matrix [[1,2],[3,4]]) <> MatrixSum (Matrix [[1,2],[3,4]]))

  , "MatrixSum addition (Bools)" ~:
      MatrixSum (Matrix [[False, False], [False, False]]) ~=? 
      (MatrixSum (Matrix [[True, False], [False, True]]) <> MatrixSum (Matrix [[True, False], [False, True]]))
  ]

-- MatrixMult tests
matrixMultTests :: Test
matrixMultTests = TestList
  [ "MatrixMult multiplication (Ints)" ~:
      MatrixMult (Matrix [[1,1,2],[2,4,5],[0,2,1]]) ~=? 
      (MatrixMult (Matrix [[1,0],[2,1],[0,1]]) <> MatrixMult (Matrix [[1,1,2],[0,2,1]]))

  , "MatrixMult symbolic expression" ~:
      (MatrixMult (Matrix [[Plus (Lit 0) (Mult (Lit 1) (Iden "x"))]]) :: MatrixMult (Expression Integer)) ~=? 
      ((MatrixMult (Matrix [[Lit 1]]) :: MatrixMult (Expression Integer)) 
        <> 
       (MatrixMult (Matrix [[Iden "x"]]) :: MatrixMult (Expression Integer)))
  ]

-- SparseMatrixSum tests
sparseSumTests :: Test
sparseSumTests = TestList
  [ "SparseMatrixSum addition" ~:
      SparseMatrixSum (SparseMatrix 3 3 (fromList [((0,0),4), ((1,1),2), ((2,2),4)])) ~=? 
      (SparseMatrixSum (SparseMatrix 3 3 (fromList [((0,0),1), ((1,1),2)])) <>
       SparseMatrixSum (SparseMatrix 3 3 (fromList [((0,0),3), ((2,2),4)])))
  ]

-- SparseMatrixMult tests
sparseMultTests :: Test
sparseMultTests = TestList
  [ "SparseMatrixMult multiplication" ~:
      SparseMatrixMult (SparseMatrix 2 2 (fromList
        [ ((0,0),19), ((0,1),22), ((1,0),43), ((1,1),50)])) ~=? 
      (SparseMatrixMult (SparseMatrix 2 2 (fromList
        [((0,0),1), ((0,1),2), ((1,0),3), ((1,1),4)])) <>
       SparseMatrixMult (SparseMatrix 2 2 (fromList
        [((0,0),5), ((0,1),6), ((1,0),7), ((1,1),8)])))
  ]

-- Adjacency matrix representing the graph:
-- A → B, A → C
-- B → C, B → D
-- C → D
-- D has no outgoing flights
adj :: Matrix Int
adj = Matrix
  [ [0, 1, 1, 0]
  , [0, 0, 1, 1]
  , [0, 0, 0, 1]
  , [0, 0, 0, 0]
  ]

-- EvalPoly tests
evalPolyTests :: Test
evalPolyTests = TestList
  [ "evalPoly [1,2,3] 2" ~:
      (17 :: Integer) ~=? evalPoly [1,2,3] (2 :: Integer)
  , "evalPoly [] 5" ~:
      (0 :: Integer) ~=? evalPoly [] (5 :: Integer)
  , "evalPoly [1,0,0,3] 2" ~:
      (25 :: Integer) ~=? evalPoly [1,0,0,3] (2 :: Integer)
  , "evalPoly [True, False] True" ~:
      (True :: Bool) ~=? evalPoly [True, False] True
  , "evalPoly [False] True" ~:
      (False :: Bool) ~=? evalPoly [False] True
  , "evalPoly [Lit 2, Lit 3] (Lit 5)" ~:
      Plus (Mult (Lit 2) (Lit 1))
           (Plus (Mult (Lit 3) (Mult (Lit 5) (Lit 1))) (Lit 0))
        ~=? evalPoly [Lit 2, Lit 3] (Lit 5)
  ]


-- PathsOfLengthK tests using `adj`
pathsOfLengthKTests :: Test
pathsOfLengthKTests = TestList
  [ "pathsOfLengthK 1 1 2 adj" ~: 1 ~=? pathsOfLengthK 1 1 2 adj
  , "pathsOfLengthK 2 0 3 adj" ~: 2 ~=? pathsOfLengthK 2 0 3 adj
  ]

-- HasPath tests using `adj`
hasPathTests :: Test
hasPathTests = TestList
  [ "hasPath 0 3 adj" ~: True ~=? hasPath 0 3 adj
  , "hasPath 3 0 adj" ~: False ~=? hasPath 3 0 adj
  ]
-- Simplify tests
simplifyTests :: Test
simplifyTests = TestList
  [ "simplify Plus 0 x" ~: Iden "x" ~=? simplify (Plus (Lit 0) (Iden "x"))
  , "simplify Minus y 0" ~: Iden "y" ~=? simplify (Minus (Iden "y") (Lit 0))
  , "simplify nested Mult with Lit 1" ~:
      Plus (Lit 3) (Iden "z") ~=? simplify (Mult (Lit 1) (Plus (Lit 3) (Iden "z")))
  , "simplify Div 10 5" ~: Lit 2 ~=? simplify (Div (Lit 10) (Lit 5))
  , "simplify Div 10 0" ~: Div (Lit 10) (Lit 0) ~=? simplify (Div (Lit 10) (Lit 0))
  , "simplify Signum -7" ~: Lit (-1) ~=? simplify (Signum (Lit (-7)))
  , "simplify nested Signum-Mult-Div" ~:
      Mult (Lit (-1)) (Div (Lit (-1)) (Signum (Iden "w")))
        ~=? simplify (Signum (Mult (Lit (-3)) (Div (Lit (-5)) (Iden "w"))))
  ]

-- InlineExpressions tests
inlineExprTests :: Test
inlineExprTests = TestList
  [ "inline simple Plus" ~:
      [(Lit 5, "a")] ~=? inlineExpressions [(Plus (Lit 2) (Lit 3), "a")]
  , "inline Plus with x and e1, Mult with Plus x 0 and e2" ~:
      [(Iden "x", "e1"), (Mult (Iden "e1") (Lit 2), "e2")]
        ~=? inlineExpressions
              [(Plus (Iden "x") (Lit 0), "e1"),
               (Mult (Plus (Iden "x") (Lit 0)) (Lit 2), "e2")]
  , "inline multiple nested expressions" ~:
      [ (Plus (Iden "x") (Iden "y"), "p")
      , (Plus (Iden "z") (Iden "w"), "q")
      , (Mult (Iden "p") (Iden "q"), "r")
      ]
        ~=? inlineExpressions
              [ (Plus (Iden "x") (Iden "y"), "p")
              , (Plus (Iden "z") (Iden "w"), "q")
              , (Mult (Plus (Iden "x") (Iden "y"))
                      (Plus (Iden "z") (Iden "w")), "r")
              ]
  ]
jsonTests :: Test
jsonTests = TestList
  [ "Bool to/from JSON" ~:
      Just True ~=? (fromJson (toJson True) :: Maybe Bool)

  , "JString to/from JSON" ~:
      Just (JString "hello") ~=? (fromJson (toJson (JString "hello")) :: Maybe JString)

  , "Integer to/from JSON" ~:
      Just (42 :: Integer) ~=? (fromJson (toJson (42 :: Integer)) :: Maybe Integer)

  , "Double to/from JSON" ~:
      Just (3.14 :: Double) ~=? (fromJson (toJson (3.14 :: Double)) :: Maybe Double)

  , "Tuple (Bool, Integer)" ~:
      Just (False, 99 :: Integer)
        ~=? (fromJson (toJson (False, 99 :: Integer)) :: Maybe (Bool, Integer))

  , "Triple (Bool, JString, Integer)" ~:
      Just (True, JString "yo", 7 :: Integer)
        ~=? (fromJson (toJson (True, JString "yo", 7 :: Integer)) :: Maybe (Bool, JString, Integer))

  , "Maybe Integer (Just)" ~:
      Just (Just (123 :: Integer))
        ~=? (fromJson (toJson (Just (123 :: Integer))) :: Maybe (Maybe Integer))

  , "Maybe Integer (Nothing)" ~:
      Just Nothing ~=? (fromJson (toJson (Nothing :: Maybe Integer)) :: Maybe (Maybe Integer))

  , "Either Integer JString (Left)" ~:
      let val = (Left (10 :: Integer) :: Either Integer JString)
       in Just val ~=? (fromJson (toJson val) :: Maybe (Either Integer JString))

  , "Either Integer JString (Right)" ~:
      let val = (Right (JString "abc") :: Either Integer JString)
       in Just val ~=? (fromJson (toJson val) :: Maybe (Either Integer JString))

  , "List of Integers" ~:
      Just [1, 2, 3] ~=? (fromJson (toJson ([1,2,3] :: [Integer])) :: Maybe [Integer])

  , "List of JStrings" ~:
      Just [JString "a", JString "b"]
        ~=? (fromJson (toJson [JString "a", JString "b"]) :: Maybe [JString])
  , "MultiSet of Integer" ~:
    let ms = MS.fromList [1, 2, 2, 3] :: MS.MultiSet Integer
     in Just ms ~=? (fromJson (toJson ms) :: Maybe (MS.MultiSet Integer))

  , "MultiSet of Bool" ~:
    let ms = MS.fromList [True, False, True] :: MS.MultiSet Bool
     in Just ms ~=? (fromJson (toJson ms) :: Maybe (MS.MultiSet Bool))

  , "Tree of Integer" ~:
    let t = Tree (Tree Empty 1 Empty) 2 (Tree Empty 3 Empty) :: Tree Integer
     in Just t ~=? (fromJson (toJson t) :: Maybe (Tree Integer))

  , "Tree of Bool" ~:
    let t = Tree (Tree Empty True Empty) False (Tree Empty True Empty) :: Tree Bool
     in Just t ~=? (fromJson (toJson t) :: Maybe (Tree Bool))
  ]
  -- Tests for nested JSON conversions
nestedJsonTests :: Test
nestedJsonTests = TestList
  [ "Maybe [Integer] (Just case)" ~:
    let val = (Just [1, 2, 3] :: Maybe [Integer])
     in Just val ~=? (fromJson (toJson val) :: Maybe (Maybe [Integer]))

  , "Maybe [Integer] (Nothing case)" ~:
    let val = (Nothing :: Maybe [Integer])
     in Just val ~=? (fromJson (toJson val) :: Maybe (Maybe [Integer]))

  , "Either Bool (Maybe Integer) (Left case)" ~:
    let val = (Left True :: Either Bool (Maybe Integer))
     in Just val ~=? (fromJson (toJson val) :: Maybe (Either Bool (Maybe Integer)))

  , "Either Bool (Maybe Integer) (Right Just case)" ~:
    let val = (Right (Just 42) :: Either Bool (Maybe Integer))
     in Just val ~=? (fromJson (toJson val) :: Maybe (Either Bool (Maybe Integer)))

  , "Either Bool (Maybe Integer) (Right Nothing case)" ~:
    let val = (Right Nothing :: Either Bool (Maybe Integer))
     in Just val ~=? (fromJson (toJson val) :: Maybe (Either Bool (Maybe Integer)))

  , "Tree (Maybe Bool)" ~:
    let t = Tree 
              (Tree Empty (Just True) Empty) 
              (Just False) 
              (Tree (Tree Empty Nothing Empty) (Just True) Empty) :: Tree (Maybe Bool)
     in Just t ~=? (fromJson (toJson t) :: Maybe (Tree (Maybe Bool)))
  ]
edgeCaseJsonTests :: Test
edgeCaseJsonTests = TestList
  [ "Empty list" ~:
      Just ([] :: [Integer]) ~=? (fromJson (toJson ([] :: [Integer])) :: Maybe [Integer])
  
  , "Empty Tree" ~:
      Just (Empty :: Tree Integer) ~=? (fromJson (toJson (Empty :: Tree Integer)) :: Maybe (Tree Integer))
  
  , "List of Maybe Integer" ~:
      Just [Just 1, Nothing, Just 3] ~=? (fromJson (toJson ([Just 1, Nothing, Just 3] :: [Maybe Integer])) :: Maybe [Maybe Integer])
  
  , "MultiSet of Maybe Bool" ~:
      let ms = MS.fromList [Just True, Nothing, Just False] :: MS.MultiSet (Maybe Bool)
       in Just ms ~=? (fromJson (toJson ms) :: Maybe (MS.MultiSet (Maybe Bool)))
  
  , "Tree of Either Bool Integer" ~:
      let t = Tree (Tree Empty (Left True) Empty) (Right 42) Empty :: Tree (Either Bool Integer)
       in Just t ~=? (fromJson (toJson t) :: Maybe (Tree (Either Bool Integer)))
       
  ,  "Empty MultiSet" ~:
      let ms = MS.fromList [] :: MS.MultiSet Integer
       in Just ms ~=? (fromJson (toJson ms) :: Maybe (MS.MultiSet Integer))
  
  , "Maybe (Either Bool [Integer])" ~:
      let val = Just (Right [1,2,3]) :: Maybe (Either Bool [Integer])
       in Just val ~=? (fromJson (toJson val) :: Maybe (Maybe (Either Bool [Integer])))
  ]




-- Run all tests
allTests :: Test
allTests = TestList
  [ basicTests
  , matrixSumTests
  , matrixMultTests
  , sparseSumTests
  , sparseMultTests
  , evalPolyTests
  , pathsOfLengthKTests
  , hasPathTests
  , simplifyTests
  , inlineExprTests
  , jsonTests 
  , nestedJsonTests
  , edgeCaseJsonTests
  ]


main :: IO ()
main = runTestTT allTests >> return ()
