import Test.HUnit
import Data.Map (fromList)
import Data.List (transpose)
import HW4

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
      (Plus (Mult (Lit 2) (Lit 1))
            (Plus (Mult (Lit 3) (Mult (Lit 5) (Lit 1))) (Lit 0)) :: Expression Integer)
      ~=? (evalPoly [Lit 2, Lit 3] (Lit 5) :: Expression Integer)
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
  ]


main :: IO ()
main = runTestTT allTests >> return ()
