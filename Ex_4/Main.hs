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
  , "9 :: Bool" ~: False ~=? (9 :: Bool)
  , "42 :: Bool" ~: True ~=? (42 :: Bool)
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
  [ "MatrixMult addition (Ints)" ~:
      MatrixMult (Matrix [[1,1,2],[2,4,5],[0,2,1]]) ~=? 
      (MatrixMult (Matrix [[1,0],[2,1],[0,1]]) <> MatrixMult (Matrix [[1,1,2],[0,2,1]]))

  , "MatrixMult symbolic expression" ~:
      MatrixMult (Matrix [[Plus (Lit 1) (Mult (Lit 1) (Iden "x"))]]) ~=? 
      (MatrixMult (Matrix [[Lit 1]]) <> MatrixMult (Matrix [[Iden "x"]]))
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

-- Run all tests
allTests :: Test
allTests = TestList [basicTests, matrixSumTests, matrixMultTests, sparseSumTests, sparseMultTests]

main :: IO ()
main = runTestTT allTests >> return ()
