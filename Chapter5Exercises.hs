module Chapter5Exercises where

import Chapter5
import Test.HUnit

{- 5.1
 - Give a definition of the function
 -    maxOccurs :: Integer -> Integer -> (Integer, Integer)
 - which returns the maximum of two integers, together with the number of times it
 - occurs. Using this, or otherwise, define the function
 -    maxThreeOccurs :: Integer -> Integer -> Integer -> (Integer, Integer)
 - which does a similar thing for three arguments
-}

maxOccurs :: Integer -> Integer -> (Integer, Integer)
maxOccurs x y
  | x == y      = (x, 2)
  | otherwise   = (max x y, 1)

testMaxOccurs1 = TestCase (assertEqual "for: maxOccurs 4 4" (4,2)  (maxOccurs 4 4))
testMaxOccurs2 = TestCase (assertEqual "for: maxOccurs 4 1" (4,1)  (maxOccurs 4 1))
testMaxOccurs3 = TestCase (assertEqual "for: maxOccurs 1 4" (4,1)  (maxOccurs 1 4))
testMaxOccurs4 = TestCase (assertEqual "for: maxOccurs -1 0" (0,1) (maxOccurs (-1) 0))

testMaxOccursList = TestList [testMaxOccurs1, testMaxOccurs2, testMaxOccurs3, testMaxOccurs4]

maxOccursThree :: Integer -> Integer -> Integer -> (Integer, Integer)
maxOccursThree x y z
  | x == y && y == z                        = (x, 3)
  | (x == y || x == z) && x == maxElem      = (x, 2)
  | y == z && y == maxElem                  = (y, 2)
  | otherwise                               = (maxElem, 1)
  where
    maxElem = max (max x y) z

testMaxOccursThree1 = TestCase (assertEqual
                                "for: maxOccursThree 4 4 4" (4,3)
                               (maxOccursThree 4 4 4))
testMaxOccursThree2 = TestCase (assertEqual
                               "for: maxOccursThree 4 4 1" (4,2)
                               (maxOccursThree 4 4 1))
testMaxOccursThree3 = TestCase (assertEqual
                               "for: maxOccursThree 1 4 4" (4,2)
                               (maxOccursThree 1 4 4))
testMaxOccursThree4 = TestCase (assertEqual
                               "for: maxOccursThree 4 1 4" (4,2)
                               (maxOccursThree 4 1 4))
testMaxOccursThree5 = TestCase (assertEqual
                               "for: maxOccursThree 4 1 1" (4,1)
                               (maxOccursThree 4 1 1))
testMaxOccursThree6 = TestCase (assertEqual
                               "for: maxOccursThree 1 4 1" (4,1)
                               (maxOccursThree 1 4 1))
testMaxOccursThree7 = TestCase (assertEqual
                               "for: maxOccursThree 1 1 4" (4,1)
                               (maxOccursThree 1 1 4))

testMaxOccursThreeList = TestList [testMaxOccursThree1,
                             testMaxOccursThree2,
                             testMaxOccursThree3,
                             testMaxOccursThree4,
                             testMaxOccursThree5,
                             testMaxOccursThree6,
                             testMaxOccursThree7]




