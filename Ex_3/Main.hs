module Main where
import qualified HW3
import Test.HUnit -- Importing HUnit for unit testing

-- Define the test tree to use in tests
testTree :: HW3.Tree Int
testTree = HW3.Tree
  (HW3.Tree
    (HW3.Tree
      (HW3.Tree HW3.Empty 8 HW3.Empty)
      4
      (HW3.Tree HW3.Empty 9 HW3.Empty))
    2
    (HW3.Tree
      (HW3.Tree HW3.Empty 10 HW3.Empty)
      5
      HW3.Empty))
  1
  (HW3.Tree
    (HW3.Tree HW3.Empty 6 HW3.Empty)
    3
    (HW3.Tree HW3.Empty 7 HW3.Empty))

-- Example trees for classification tests
emptyTree :: HW3.Tree Int
emptyTree = HW3.Empty

-- Perfect tree: all internal nodes have 2 children, all leaves at same depth
perfectTree :: HW3.Tree Int
perfectTree = HW3.Tree 
  (HW3.Tree 
    (HW3.Tree HW3.Empty 4 HW3.Empty) 
    2 
    (HW3.Tree HW3.Empty 5 HW3.Empty))
  1
  (HW3.Tree 
    (HW3.Tree HW3.Empty 6 HW3.Empty) 
    3 
    (HW3.Tree HW3.Empty 7 HW3.Empty))

-- Full tree: every node has 0 or 2 children
fullTree :: HW3.Tree Int
fullTree = HW3.Tree
  (HW3.Tree 
    HW3.Empty 
    2 
    HW3.Empty)
  1
  (HW3.Tree 
    (HW3.Tree HW3.Empty 4 HW3.Empty) 
    3 
    (HW3.Tree HW3.Empty 5 HW3.Empty))

-- Complete tree: all levels except possibly the last are filled
-- and all nodes are as far left as possible
completeTree :: HW3.Tree Int
completeTree = HW3.Tree
  (HW3.Tree 
    (HW3.Tree HW3.Empty 4 HW3.Empty) 
    2 
    (HW3.Tree HW3.Empty 5 HW3.Empty)) 
  1
  (HW3.Tree
    (HW3.Tree HW3.Empty 6 HW3.Empty)  
    3
    HW3.Empty)


-- Full and complete tree: a perfect tree is both full and complete
fullAndCompleteTree :: HW3.Tree Int
fullAndCompleteTree = perfectTree

-- Degenerate tree: each node has only one child
degenerateTree :: HW3.Tree Int
degenerateTree = HW3.Tree
  HW3.Empty
  1
  (HW3.Tree
    HW3.Empty
    2
    (HW3.Tree
      HW3.Empty
      3
      HW3.Empty))

-- Other tree: doesn't fit into any specific classification
otherTree :: HW3.Tree Int
otherTree = HW3.Tree
  (HW3.Tree 
    HW3.Empty 
    2 
    (HW3.Tree HW3.Empty 4 HW3.Empty))
  1
  (HW3.Tree 
    (HW3.Tree HW3.Empty 5 HW3.Empty)
    3
    HW3.Empty)

-- Balanced tree for testing isBalanced
balancedTree :: HW3.Tree Int
balancedTree = HW3.Tree
  (HW3.Tree 
    (HW3.Tree HW3.Empty 4 HW3.Empty)
    2
    HW3.Empty)
  1
  (HW3.Tree 
    HW3.Empty
    3
    (HW3.Tree HW3.Empty 5 HW3.Empty))

-- Unbalanced tree for testing isBalanced
unbalancedTree :: HW3.Tree Int
unbalancedTree = HW3.Tree
  (HW3.Tree 
    (HW3.Tree 
      (HW3.Tree HW3.Empty 8 HW3.Empty)
      4
      HW3.Empty)
    2
    HW3.Empty)
  1
  (HW3.Tree HW3.Empty 3 HW3.Empty)

-- Testing the treeSize function
testTreeSize :: Test
testTreeSize = TestCase (assertEqual "for treeSize" 10 (HW3.treeSize testTree))

-- Testing the treeHeight function for a non-empty tree
testTreeHeight :: Test
testTreeHeight = TestCase (assertEqual "for treeHeight of testTree" 4 (HW3.treeHeight testTree))

-- Testing the treeHeight function for an empty tree
testTreeHeightEmpty :: Test
testTreeHeightEmpty = TestCase (assertEqual "for treeHeight of Empty" 0 (HW3.treeHeight HW3.Empty))

-- Testing the preOrderTraversal function
testPreOrderTraversal :: Test
testPreOrderTraversal = TestCase (assertEqual "for preOrderTraversal"
  [1,2,4,8,9,5,10,3,6,7]
  (HW3.preOrderTraversal testTree))

-- Testing the inOrderTraversal function
testInOrderTraversal :: Test
testInOrderTraversal = TestCase (assertEqual "for inOrderTraversal"
  [8,4,9,2,10,5,1,6,3,7]
  (HW3.inOrderTraversal testTree))

-- Testing the postOrderTraversal function
testPostOrderTraversal :: Test
testPostOrderTraversal = TestCase (assertEqual "for postOrderTraversal"
  [8,9,4,10,5,2,6,7,3,1]
  (HW3.postOrderTraversal testTree))

-- Testing the isFull function
testIsFull :: Test
testIsFull = TestList [
  TestCase (assertEqual "Empty tree is full" True (HW3.isFull emptyTree)),
  TestCase (assertEqual "Perfect tree is full" True (HW3.isFull perfectTree)),
  TestCase (assertEqual "Full tree is full" True (HW3.isFull fullTree)),
  TestCase (assertEqual "Complete tree is not necessarily full" False (HW3.isFull completeTree)),
  TestCase (assertEqual "Degenerate tree is not full" False (HW3.isFull degenerateTree)),
  TestCase (assertEqual "Other tree is not full" False (HW3.isFull otherTree))
  ]

-- Testing the isComplete function
testIsComplete :: Test
testIsComplete = TestList [
  TestCase (assertEqual "Empty tree is complete" True (HW3.isComplete emptyTree)),
  TestCase (assertEqual "Perfect tree is complete" True (HW3.isComplete perfectTree)),
  TestCase (assertEqual "Full tree is not necessarily complete" False (HW3.isComplete fullTree)),
  TestCase (assertEqual "Complete tree is complete" True (HW3.isComplete completeTree)),
  TestCase (assertEqual "Degenerate tree is not complete" False (HW3.isComplete degenerateTree)),
  TestCase (assertEqual "Other tree is not complete" False (HW3.isComplete otherTree))
  ]

-- Testing the isPerfect function
testIsPerfect :: Test
testIsPerfect = TestList [
  TestCase (assertEqual "Empty tree is perfect" True (HW3.isPerfect emptyTree)),
  TestCase (assertEqual "Perfect tree is perfect" True (HW3.isPerfect perfectTree)),
  TestCase (assertEqual "Full tree is not necessarily perfect" False (HW3.isPerfect fullTree)),
  TestCase (assertEqual "Complete tree is not necessarily perfect" False (HW3.isPerfect completeTree)),
  TestCase (assertEqual "Degenerate tree is not perfect" False (HW3.isPerfect degenerateTree)),
  TestCase (assertEqual "Other tree is not perfect" False (HW3.isPerfect otherTree))
  ]

-- Testing the isDegenerate function
testIsDegenerate :: Test
testIsDegenerate = TestList [
  TestCase (assertEqual "Empty tree is degenerate" True (HW3.isDegenerate emptyTree)),
  TestCase (assertEqual "Perfect tree is not degenerate" False (HW3.isDegenerate perfectTree)),
  TestCase (assertEqual "Full tree is not degenerate" False (HW3.isDegenerate fullTree)),
  TestCase (assertEqual "Complete tree is not degenerate" False (HW3.isDegenerate completeTree)),
  TestCase (assertEqual "Degenerate tree is degenerate" True (HW3.isDegenerate degenerateTree)),
  TestCase (assertEqual "Other tree is not degenerate" False (HW3.isDegenerate otherTree))
  ]

-- Testing the classify function
testClassify :: Test
testClassify = TestList [
  TestCase (assertEqual "Empty tree classification" HW3.Perfect (HW3.classify emptyTree)),
  TestCase (assertEqual "Perfect tree classification" HW3.Perfect (HW3.classify perfectTree)),
  TestCase (assertEqual "Full tree classification" HW3.Full (HW3.classify fullTree)),
  TestCase (assertEqual "Complete tree classification" HW3.Complete (HW3.classify completeTree)),
  TestCase (assertEqual "Full and complete tree classification" HW3.Perfect (HW3.classify fullAndCompleteTree)),
  TestCase (assertEqual "Degenerate tree classification" HW3.Degenerate (HW3.classify degenerateTree)),
  TestCase (assertEqual "Other tree classification" HW3.Other (HW3.classify otherTree))
  ]

-- Testing the isBalanced function
testIsBalanced :: Test
testIsBalanced = TestList [
  TestCase (assertEqual "Empty tree is balanced" True (HW3.isBalanced emptyTree)),
  TestCase (assertEqual "Perfect tree is balanced" True (HW3.isBalanced perfectTree)),
  TestCase (assertEqual "Balanced tree is balanced" True (HW3.isBalanced balancedTree)),
  TestCase (assertEqual "Unbalanced tree is not balanced" False (HW3.isBalanced unbalancedTree)),
  TestCase (assertEqual "Degenerate tree is not balanced (if height > 1)" False (HW3.isBalanced degenerateTree))
  ]

-- Combine all tests into a test list
tests :: Test
tests = TestList [
  testTreeSize,
  testTreeHeight,
  testTreeHeightEmpty,
  testPreOrderTraversal,
  testInOrderTraversal,
  testPostOrderTraversal,
  testIsFull,
  testIsComplete,
  testIsPerfect,
  testIsDegenerate,
  testClassify,
  testIsBalanced
  ]

-- Run the tests
main :: IO Counts
main = runTestTT tests