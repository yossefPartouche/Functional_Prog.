-- Add at the top if not already present
import HW5
import Prelude hiding (sum, product, length, null)

-- HW5 Fold tests
foldTests :: Test
foldTests = TestList
  [ "sumF" ~: runFold sumF [1,2,3,4] ~=? 10
  , "productF" ~: runFold productF [1,2,3,4] ~=? 24
  , "lengthF" ~: runFold lengthF "hello" ~=? 5
  , "averageF" ~: runFold averageF [2.0, 4.0, 6.0] ~=? 4.0
  , "nullF empty" ~: runFold nullF ([] :: [Int]) ~=? True
  , "nullF non-empty" ~: runFold nullF [1] ~=? False
  , "findF hits even" ~: runFold (findF even) [1,3,4,5] ~=? Just 4
  , "findF misses all" ~: runFold (findF (> 100)) [1..10] ~=? Nothing
  , "topKF k=3" ~: runFold (topKF 3) [5,1,9,2,7] ~=? [9,7,5]
  , "topKF k > length" ~: runFold (topKF 10) [4,3] ~=? [4,3]
  , "mapF square" ~: runFold (mapF (^2) sumF) [1,2,3] ~=? 14
  , "filterF even sum" ~: runFold (filterF even sumF) [1..5] ~=? 6
  ]
