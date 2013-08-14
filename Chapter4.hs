--------------------------------------------------------------------------
--
-- 	Haskell: The Craft of Functional Programming, 3e
-- 	Simon Thompson
-- 	(c) Addison-Wesley, 1996-2011.
-- 
-- 	Chapter 4
--
--------------------------------------------------------------------------

-- NOTE
--
-- Added HUnit and QuickCheck tests
--
-- HUnit 1.0 documentation is out of date
-- re package name.

module Chapter4 where

import Test.HUnit
import Test.QuickCheck
import PicturesSVG hiding (test2)

-- Designing a program in Haskell
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

maxThree :: Int -> Int -> Int -> Int
maxThree x y z = (x `max` y) `max` z

testMax1 = TestCase (assertEqual "for: maxThree 6 4 1" 6 (maxThree 6 4 1))
testMax2 = TestCase (assertEqual "for: maxThree 6 6 6" 6 (maxThree 6 6 6))
testMax3 = TestCase (assertEqual "for: maxThree 2 6 6" 6 (maxThree 2 6 6))
testMax4 = TestCase (assertEqual "for: maxThree 2 2 6" 6 (maxThree 2 2 6))

-- run as 
--   runTestTT testsMax

testsMax = TestList [testMax1, testMax2, testMax3, testMax4]

-- NOTE
--
-- Added this type synonym so that can switch easily
-- between Integer and Int.

type MyNum = Integer

middleNumber :: MyNum -> MyNum -> MyNum -> MyNum
middleNumber x y z
  | between y x z      = x
  | between x y z      = y
  | otherwise          = z

-- What follows here is a dummy definition of between; you need to replace this
-- with a proper definition for the function middleNumber to work.

between ::  MyNum -> MyNum -> MyNum -> Bool

-- dummy definition 
-- for you to complete!

-- NOTE
--
-- HUnit tests added
--
-- To run evaluate: runTestTT tests

test1 = TestCase (assertEqual "for: between 2 3 4" True (between 2 3 4))
test2 = TestCase (assertEqual "for: between 2 3 2" False (between 2 3 2))
test3 = TestCase (assertEqual "for: between 2 3 3" True (between 2 3 3))
test4 = TestCase (assertEqual "for: between 3 3 3" True (between 3 3 3))
test5 = TestCase (assertEqual "for: between 3 2 3" False (between 3 2 3))
test6 = TestCase (assertEqual "for: between 3 2 1" True (between 3 2 1))

testsBetween = TestList [test1, test2, test3, test4, test5, test6]

-- NOTE
-- 
-- Interesting to vary the implementation and see which tests fail.
-- Simple form of mutation testing.

-- QuickCheck test
--
-- Does the tricky implementation of between work in the 
-- same way as the case analysis?

prop_between :: MyNum -> MyNum -> MyNum -> Bool

prop_between x y z 
 = (between x y z) == ((x<=y)&&(y<=z))||((x>=y)&&(y>=z))

-- Unit tests as Quick Check properties

prop_between1 :: Bool

prop_between1
 = between 2 3 4 == True

-- Local definitions
-- ^^^^^^^^^^^^^^^^^

-- Four ways of defining a Picture using 
-- different combinations of loca definitions.


fourPics1 :: Picture -> Picture

fourPics1 pic =
    left `beside` right
      where
        left  = pic `above` invertColour pic
        right = invertColour (flipV pic) `above` flipV pic

fourPics2 :: Picture -> Picture
fourPics2 pic =
    left `beside` right
      where
        left    = pic `above` invertColour pic
        right   = invertColour flipped `above` flipped
        flipped = flipV pic

fourPics3 :: Picture -> Picture

fourPics3 pic =
    left `beside` right
      where
        left  = pic `above` invertColour pic
        right = invertColour (flipV left)

fourPics4 :: Picture -> Picture

fourPics4 pic =
    left `beside` right
      where
        stack p  = p `above` invertColour p
        left     = stack pic
        right    = stack (invertColour (flipV pic))

-- Area of a triangle

positive :: Float -> Bool
positive x = x > 0

threePositive :: Float -> Float -> Float -> Bool
threePositive a b c = positive a && positive b && positive c

triInequality :: Float -> Float -> Float -> Bool
triInequality a b c = (a + b) > c && (a + c) > b && (b + c) > a

triArea' :: Float -> Float -> Float -> Float

triArea' a b c 
    | possible   = sqrt(s*(s-a)*(s-b)*(s-c))
    | otherwise  = 0
    where
      s = (a+b+c)/2 
      possible = threePositive a b c && triInequality a b c

-- Sum of squares

sumSquares :: Integer -> Integer -> Integer

sumSquares n m 
  = sqN + sqM
    where
    sqN = n*n
    sqM = m*m


-- Let expressions
-- ^^^^^^^^^^^^^^^

-- Two examples which use `let'.

letEx1 :: Integer
letEx1 = let x = 3+2 in x^2 + 2*x - 4

letEx2 :: Integer
letEx2 = let x = 3+2 ; y = 5-1 in x^2 + 2*x - y


-- Scopes

isOdd, isEven :: Int -> Bool

isOdd n 
  | n<=0        = False
  | otherwise   = isEven (n-1)

isEven n 
  | n<0         = False
  | n==0        = True
  | otherwise   = isOdd (n-1)


-- Defining types for ourselves
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^

-- Rock - Paper - Scissors

data Move = Rock | 
            Paper | 
            Scissors
            deriving Eq

-- Showing Moves in an abbreviated form.

instance Show Move where
      show Rock = "r"
      show Paper = "p"
      show Scissors = "s"

-- For QuickCheck to work over the Move type.

instance Arbitrary Move where
  arbitrary     = elements [Rock, Paper, Scissors]

-- Calculating the Move to beat or lose against the 
-- argument Move.

beat, lose :: Move -> Move

beat Rock = Paper
beat Paper = Scissors
beat Scissors = Rock

lose Rock = Scissors
lose Paper = Rock
lose Scissors = Paper


-- Primitive recursion over Int
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^

-- The factorial of n is 1*2*...*(n-1)*n, so that factorial of four is 24.
-- It is often written n!

fac :: Integer -> Integer
fac n
  | n==0        = 1
  | n>0         = fac (n-1) * n
  | otherwise   = error "fac only defined on natural numbers"

--                                      n
-- Raising two to a power: power2 n is 2  in mathematical notation.

power2 :: Integer -> Integer
power2 n
  | n==0        = 1
  | n>0         = 2 * power2 (n-1)

-- The sum of the factorials up to a particular value, 0! + 1! + ... n!.

sumFacs :: Integer -> Integer
sumFacs n
  | n==0        = 1
  | n>0         = sumFacs (n-1) + fac n  

-- The sum of the values of a function up to a particular value: 
-- 	f 0 + f 1 + ... f n
-- from which you can reconstruct sumFacs: sumFacs n = sumFun fac n

sumFun :: (Integer -> Integer) -> Integer -> Integer
sumFun f n
  | n==0        = f 0
  | n>0         = sumFun f (n-1) + f n  

-- The maximum number of regions into which n lines can cut a plane.

regions :: Integer -> Integer 
regions n
  | n==0        = 1
  | n>0         = regions (n-1) + n

-- The Fibonacci numbers 0, 1, 1, 2, 3, 5, ..., u, v, u+v, ...

fib :: Integer -> Integer
fib n 
  | n==0        = 0
  | n==1        = 1
  | n>1         = fib (n-2) + fib (n-1)

-- Division of integers

remainder :: Integer -> Integer -> Integer
remainder m n 
  | m<n         = m
  | otherwise   = remainder (m-n) n

divide    :: Integer -> Integer -> Integer
divide m n
  | m<n         = 0
  | otherwise   = 1 + divide (m-n) n

-- Testing
-- ^^^^^^^

-- Does this function calculate the maximum of three numbers?

mysteryMax :: Integer -> Integer -> Integer -> Integer
mysteryMax x y z
  | x > y && x > z      = x
  | y > x && y > z      = y
  | otherwise           = z

testMMax1 = TestCase (assertEqual "for: mysteryMax 6 4 1" 6 (mysteryMax 6 4 1))
testMMax2 = TestCase (assertEqual "for: mysteryMax 6 6 6" 6 (mysteryMax 6 6 6))
testMMax3 = TestCase (assertEqual "for: mysteryMax 2 6 6" 6 (mysteryMax 2 6 6))
testMMax4 = TestCase (assertEqual "for: mysteryMax 2 2 6" 6 (mysteryMax 2 2 6))
testMMax5 = TestCase (assertEqual "for: mysteryMax 6 6 2" 6 (mysteryMax 6 6 2))


testsMMax = TestList [testMMax1, testMMax2, testMMax3, testMMax4, testMMax5]


-- Numbers of roots

numberNDroots :: Float -> Float -> Float -> Integer 

numberNDroots a b c
    | bsq > fac   = 2
    | bsq == fac  = 1
    | bsq < fac   = 0
    where
      bsq = b*b
      fac = 4.0*a*c 

-- Area of a triangle

triArea :: Float -> Float -> Float -> Float

triArea a b c 
    | possible a b c = sqrt(s*(s-a)*(s-b)*(s-c))
    | otherwise      = 0
    where
      s = (a+b+c)/2 

possible :: Float -> Float -> Float -> Bool

possible a b c = True -- dummy definition

fact :: Int -> Int
  
fact n 
    | n>1       = n * fact (n-1)
    | otherwise = 1

prop_fact n =
  fact n > 0

-- Extended exercise
-- ^^^^^^^^^^^^^^^^^

blackSquares :: Integer -> Picture

blackSquares n
  | n<=1	     = black
  | otherwise = black `beside` blackSquares (n-1)

blackWhite :: Integer -> Picture

blackWhite n
  | n<=1	     = black
  | otherwise = black `beside` whiteBlack (n-1)

whiteBlack n
  | n<=1             = white
  | otherwise = white `beside` blackWhite (n-1)

blackChess :: Integer -> Integer -> Picture

blackChess n m
  | n<=1	     = blackWhite m
  | otherwise = blackWhite m `above` whiteChess (n-1) m

whiteChess n m
  | n<=1             = whiteBlack m
  | otherwise        = whiteBlack m `above` blackChess (n-1) m

-- 4.1

maxFour1, maxFour2, maxFour3 :: Int -> Int -> Int -> Int -> Int

maxFour1 a b c d = max (max a b) (max c d)
maxFour2 a b c d = max (maxThree a b c) d
maxFour3 a b c d = max (max (max a b) c) d

prop_maxFour :: Int -> Int -> Int -> Int -> Bool
prop_maxFour a b c d = 
  maxFour1 a b c d == maxFour2 a b c d
  && maxFour2 a b c d == maxFour3 a b c d

weakAscendingOrder :: Integer -> Integer -> Integer -> Bool
weakAscendingOrder a b c = (a <= b) && (b <= c)

weakDescendingOrder :: Integer -> Integer -> Integer -> Bool
weakDescendingOrder a b c = (c <= b) && (b <= a)

between a b c = weakAscendingOrder a b c || weakDescendingOrder a b c

threeEqual :: Integer -> Integer -> Integer -> Bool
threeEqual m n p = (m==n) && (n==p)

twoEqual :: Integer -> Integer -> Integer -> Bool
twoEqual m n p = ((m==n) || (m==p) || (n==p)) && not (threeEqual m n p)

howManyEqual :: Integer -> Integer -> Integer -> Integer
howManyEqual a b c
  |threeEqual a b c     = 3
  |twoEqual a b c       = 2
  |otherwise            = 0

fourEqual, threeEqual2, twoEqual2 :: Integer -> Integer -> Integer -> Integer -> Bool

fourEqual a b c d = (a == b) && (b == c) && (c == d)
threeEqual2 a b c d = not (fourEqual a b c d) && (threeEqual a b c
  || threeEqual b c d
  || threeEqual a c d
  || threeEqual a b d)

twoEqual2 a b c d = not (fourEqual a b c d) && not (threeEqual2 a b c d) &&
  (twoEqual a b c || twoEqual b c d || twoEqual a c d || twoEqual a b d)

howManyOfFourEqual :: Integer -> Integer -> Integer -> Integer -> Integer
howManyOfFourEqual a b c d
  |fourEqual a b c d        = 4
  |threeEqual2 a b c d      = 3
  |twoEqual2 a b c d        = 2
  |otherwise                = 0

fourPics5 :: Picture -> Picture
fourPics5 p = 
  top `above` bottom
  where
  top = p `beside` invertColour (flipV p)
  bottom = flipV top

fourPics6 :: Picture -> Picture
fourPics6 p =
  top `above` bottom
  where
  reflectH = flipV p
  inverseReflect = invertColour reflectH
  top = p `beside` inverseReflect
  bottom = invertColour top

maxThreeOccurs :: Int -> Int -> Int -> (Int, Int)
maxThreeOccurs a b c
  |threeEqual n1 n2 n3     = (m, 3)
  |(a==b && a==m) || (b==c && b==m) || (a==c && c==m)   = (m, 2)
  |otherwise            = (m, 1)
  where
  n1 = fromIntegral a
  n2 = fromIntegral b
  n3 = fromIntegral c
  m = maxThree a b c

data MyResult = Win | Lose | Draw
     deriving (Show, Eq)

outcome :: Move -> Move -> MyResult
outcome x y
  |x == y           = Draw
  |(x == beat y)    = Win
  |(x == lose y)    = Lose

data Temp = Cold | Hot
     deriving (Eq, Show, Ord)

data Season = Spring | Summer | Winter | Autumn
     deriving (Eq, Show)

tempFromSeason :: Season -> Temp
tempFromSeason season
               | season == Winter   = Cold
               | season == Summer   = Hot
               | season == Autumn   = Cold
               | season == Spring   = Cold

data Month = January
     | February
     | March
     | April
     | May
     | June
     | July
     | August
     | September
     | October
     | November
     | December
     deriving (Show, Eq, Ord)

isSpring, isSummer, isAutumn, isWinter :: Month -> Bool
isWinter m = (m == December) || (m == January) || (m == February)
isSpring m = (m == March) || (m == April) || (m == May)
isSummer m = (m == June) || (m == July) || (m == August)
isAutumn m = (m == September) || (m == October) || (m == November)

whatSeason :: Month -> Season
whatSeason m
           |isSpring m    = Spring
           |isSummer m   = Summer
           |isAutumn m   = Autumn
           |isWinter m   = Winter

-- Find product: m * (m+1) * ... * (n-1) * n
rangeProduct :: Integer -> Integer -> Integer
rangeProduct m n
             |(n < m)       = 0
             |(m == n)      = m
             |(m+1 == n)    = m * n
             |otherwise     = m * rangeProduct (m+1) (n-1) * n

fac2 :: Integer -> Integer
fac2 x
    |(x < 0)            = 0
    |(x==0 || x==1)     = x
    |otherwise          = rangeProduct 1 x

sumN :: Integer -> Integer
sumN n
     |n==0      = 0
     |otherwise = sumN (n-1) + n

productN :: Integer -> Integer
productN n
        |n==1        = 1
        |otherwise   = productN (n-1) * n

square :: Integer -> Integer
square n = n * n

mySqrt :: Integer -> Integer
mySqrt n = sqrtIter n 1

-- Recursive definition of a function to find highest integer sqrt
sqrtIter :: Integer -> Integer -> Integer
sqrtIter n guess
         |(square guess == n)   = guess
         |(square next) > n     = guess
         |otherwise             = sqrtIter n next
         where
         next = guess + 1


{-

Given a function f of type Integer -> Integer give a recursive definition
of a function of type Integer -> Integer which on input n returns the
maximum of the values f 0, f1, ..., f n. You might find the max function
defined in section 3.4 useful.

To test this function, add to your script a definition of some values of f thus:

f 0 = 0
f 1 = 44
f 2 = 17
f _ = 0

and so on; then test your function at various values.

-}

testFun 0 = 0
testFun 1 = 44
testFun 2 = 17

maxFun :: (Integer -> Integer) -> Integer -> Integer -> Integer
maxFun f n maxN
       |(n==0)      = max (f 0) maxN
       |otherwise   = maxFun f (n-1) (max (f n) maxN)

zeroFun :: (Integer -> Integer) -> Integer -> Bool
zeroFun f n
        |(n==0)     = f n == 0
        |(f n == 0) = True
        |otherwise  = zeroFun f (n-1)

picColumn :: Picture -> Integer -> Picture
picColumn pic n
  | n<=1      = pic
  | otherwise = pic `above` (picColumn pic (n-1))

