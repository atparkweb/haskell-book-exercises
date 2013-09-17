module Chapter5Exercises where

import Chapter3
import Prelude hiding (max, min)
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

{- 5.2
 - Give a definition of a function
 -    orderTriple :: (Integer, Integer, Integer) -> (Integer, Integer, Integer)
 - which puts the elements of a triple of three integers into ascending order.
 - You might like to use the maxThree, middle and minThree functions defined earlier.
-}

weakAscendingOrder :: Integer -> Integer -> Integer -> Bool
weakAscendingOrder a b c = (a <= b) && (b <= c)

weakDescendingOrder :: Integer -> Integer -> Integer -> Bool
weakDescendingOrder a b c = (c <= b) && (b <= a)

between a b c = weakAscendingOrder a b c || weakDescendingOrder a b c

middleNumber :: Integer -> Integer -> Integer -> Integer
middleNumber x y z
  | between y x z      = x
  | between x y z      = y
  | otherwise          = z

orderTriple :: (Integer, Integer, Integer) -> (Integer, Integer, Integer)
orderTriple (x, y, z) = (minThree x y z, middleNumber x y z, maxThree x y z)

testOrderTriple1 = TestCase(assertEqual
                            "for: orderTriple (1, 1, 1)"
                            (1,1,1) (orderTriple (1,1,1)))
testOrderTriple2 = TestCase(assertEqual
                            "for: orderTriple (1, 2, 3)"
                            (1,2,3) (orderTriple (1,2,3)))
testOrderTriple3 = TestCase(assertEqual
                            "for: orderTriple (1, 3, 2)"
                            (1,2,3) (orderTriple (1,3,2)))
testOrderTriple4 = TestCase(assertEqual
                            "for: orderTriple (3, 2, 1)"
                            (1,2,3) (orderTriple (3,2,1)))
testOrderTriple5 = TestCase(assertEqual
                            "for: orderTriple (1, 1, 2)"
                            (1,1,2) (orderTriple (1,1,2)))
testOrderTriple6 = TestCase(assertEqual
                            "for: orderTriple (1, 2, 1)"
                            (1,1,2) (orderTriple (1,2,1)))
testOrderTriple7 = TestCase(assertEqual
                            "for: orderTriple (2, 1, 1)"
                            (1,1,2) (orderTriple (2,1,1)))
testOrderTriple8 = TestCase(assertEqual
                            "for: orderTriple (0,(-1), 1)"
                            ((-1),0,1) (orderTriple (0,(-1),1)))

testOrderTriple = TestList [testOrderTriple1,
                            testOrderTriple2,
                            testOrderTriple3,
                            testOrderTriple4,
                            testOrderTriple5,
                            testOrderTriple6,
                            testOrderTriple7,
                            testOrderTriple8]

{- 5.3
 - Define the function which finds where a straight line crosses the x-axis. You will
 - need to think about how to supply the information about the straight line to the
 - function.
-}

-- Point is defined by an x and y coordinate
type Point = (Float, Float)

-- Straight line is defined by coordinates of 2 Points ((x1, y1), (x2, y2))
type Line = (Point, Point)

getX :: Point -> Float
getX p = fst p

getY :: Point -> Float
getY p = snd p

deltaX :: Line -> Float
deltaX line = x1 - x2
    where x1 = getX (fst line)
          x2 = getX (snd line)

deltaY :: Line -> Float
deltaY line = y1 - y2
    where y1 = getY (fst line)
          y2 = getY (snd line)

slope :: Line -> Float
slope line = dy / dx
    where dy = deltaY line
          dx = deltaX line

getB :: Line -> Float
getB line = y - m * x
    where y  = getY p1
          x  = getX p1
          m  = slope line
          p1 = (fst line)

xIntercept :: Line -> Float
xIntercept line
    |(deltaX line == 0) = getX (fst line)
    |otherwise          = (-b)/m
    where b = getB line
          m = slope line

{- 5.4
 - Define test data for the preceding exercises; explain the choices you have made
 - in each case. Give a sample evaluation of each of your functions."
-}

-- Test with vertical line (i.e. slope = NaN)
testXIntercept1 = TestCase(assertEqual "for xIntercept: (2,1), (2,4)"
                                       2.0
                                       (xIntercept ((2,1),(2,1))))

-- Test with zero for x and y values for both points
testXIntercept2 = TestCase(assertEqual "for xIntercept: (0,0), (0,0)"
                                       0.0
                                       (xIntercept ((0,0),(0,0))))

-- Test with negative value
testXIntercept3 = TestCase(assertEqual "for xIntercept: (-1,2), (4, 3)"
                                       (-11.0)
                                       (xIntercept ((-1,2),(4,3))))

-- Test with line parallel to x-axis (i.e. no intersection with x-axis)
testXIntercept4 = TestCase(assertEqual "for xIntercept: (1,2), (4,2)"
                                       (1/0)
                                       (xIntercept ((1,2),(4,2))))

-- Test with points within standard range
testXIntercept5 = TestCase(assertEqual "for xIntercept: (1,2), (3,4)"
                                       0.0
                                       (xIntercept ((1,2),(3,4))))

testIntercept = TestList [testXIntercept1,
                          testXIntercept2,
                          testXIntercept3,
                          testXIntercept4,
                          testXIntercept5]