{- |
 - Module       : Chapter4Exercises
 - Description  : Exercises from the book 
 -                Haskell: The Craft of Functional Programming 3rd Edition
 -                by Simon Thompson
 - Copyright    : (c) Andy Park
 - License      : The MIT License (MIT) (http://opensource.org/licenses/MIT)
 -
-}

module Chapter4Exercises where

import Chapter4
import PicturesSVG

{- 4.9
 - Define the function
     maxThreeOccurs :: Int -> Int -> Int -> (Int, Int)
 - which returns the maximum number of three integers paired with the number
 - of times it occurs among the three. A natural solution first finds the max,
 - and then investigates how often it occurs among the three. Discuss how you
 - would write your solution if you were not allowed to use where-definitions.
-}

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

{- 4.11
 - Define a type Result which represents the outcome of a round of Rock-Paper-Scissors,
 - which will wither be a win, lose, or draw.
-}

data MyResult = Win | Lose | Draw
     deriving (Show, Eq)

{- 4.12
 - Define a function
    outcome :: Move -> Move -> Result
 - so that this gives the outcome of a round for the first player. For example, we
 - should expect that outcome Rock Scissors should be a win.
-}

outcome :: Move -> Move -> MyResult
outcome x y
  |x == y           = Draw
  |(x == beat y)    = Win
  |(x == lose y)    = Lose

{- 4.15
 - Define a type of seasons, Season, and give a function from seasons to temperature
 - given by type Temp
 - In defining this function assume that you're in the UK
-}

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

{- 4.16
 - Define a type Month and a function from this type to Season, assuming that
 - you are in the northern hemisphere.
-}

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
           |isSpring m   = Spring
           |isSummer m   = Summer
           |isAutumn m   = Autumn
           |isWinter m   = Winter

{- 4.17
 - Define the function rangeProduct which when given natural numbers m and n
 - returns the product:
 -   m * (m+1) * ... * (n-1) * n
 - You should include in your definition the type of the function, and your
 - function should return 0 when n is smaller than m.

 - Hint: you do not need to use recursion in your definition, but you may
 -       if you wish.
-}

rangeProduct :: Integer -> Integer -> Integer
rangeProduct m n
             |(n < m)       = 0
             |(m == n)      = m
             |(m+1 == n)    = m * n
             |otherwise     = m * rangeProduct (m+1) (n-1) * n

{- 4.18
 - As fac is a special case of rangeProduct, write a definition of fac which
 - uses rangeProduct.
-}
fac2 :: Integer -> Integer
fac2 x
    |(x < 0)            = 0
    |(x==0 || x==1)     = x
    |otherwise          = rangeProduct 1 x

{- 4.20
 - The integer square root of a positive integer n is the largest integer whose
 - square is less than or equal to n. For instance, the integer square roots of
 - 15 and 16 are 3 and 4, respectively. Give a primitive recursive definition of
 - this function.
-}
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

{- 4.21
 - Given a function f of type Integer -> Integer give a recursive definition
 - of a function of type Integer -> Integer which on input n returns the
 - maximum of the values f 0, f1, ..., f n. You might find the max function
 - defined in section 3.4 useful.

 - To test this function, add to your script a definition of some values of f thus:

 - f 0 = 0
 - f 1 = 44
 - f 2 = 17
 - f _ = 0

 - and so on; then test your function at various values.
-}

testFun 0 = 0
testFun 1 = 44
testFun 2 = 17

maxFun :: (Integer -> Integer) -> Integer -> Integer -> Integer
maxFun f n maxN
       |(n==0)      = max (f 0) maxN
       |otherwise   = maxFun f (n-1) (max (f n) maxN)

{- 4.22
 - Given a function f of type Integer -> Integer give a recursive definition of a
 - function of type Integer -> Bool which on input n returns True if one or more
 - of the values f 0, f 1, ..., f n is zero and False otherwise
-}

zeroFun :: (Integer -> Integer) -> Integer -> Bool
zeroFun f n
        |(n==0)     = f n == 0
        |(f n == 0) = True
        |otherwise  = zeroFun f (n-1)


-- Extended Exercises

{- 4.25
 - Complete the definitions of whiteBlack and whiteChess.
-}

blackWhite :: Integer -> Picture

blackWhite n
  | n<=1	     = black
  | otherwise = black `beside` whiteBlack (n-1)

blackChess :: Integer -> Integer -> Picture

blackChess n m
  | n<=1	     = blackWhite m
  | otherwise = blackWhite m `above` whiteChess (n-1) m

whiteBlack :: Integer -> Picture
whiteBlack n
  | n<=1             = white
  | otherwise = white `beside` blackWhite (n-1)


whiteChess :: Integer -> Integer -> Picture
whiteChess n m
  | n<=1             = whiteBlack m
  | otherwise        = whiteBlack m `above` blackChess (n-1) m

{- 4.26
 - How would you define a function to give a column of n pictures?
-}

picColumn :: Picture -> Integer -> Picture
picColumn pic n
  | n<=1      = pic
  | otherwise = pic `above` (picColumn pic (n-1))

-- Functions for exercises 4.27-29
nSquares :: Integer -> Picture -> (Picture -> Picture -> Picture) -> Picture
nSquares n pic positionFn
  | n <= 1     = pic
  | otherwise  = positionFn pic (nSquares (n-1) pic positionFn)

blackColumn :: Integer -> Picture
blackColumn n = nSquares n black above

blackRow :: Integer -> Picture
blackRow n = nSquares n black beside

whiteColumn :: Integer -> Picture
whiteColumn n = nSquares n white above

whiteRow :: Integer -> Picture
whiteRow n = nSquares n white beside

{- 4.27
 - Give a haskell function which takes an integer n and returns an n by n white
 - square with a diagonal black line from top left to bottom right as in:
 - #___
 - _#__
 - __#_
 - ___#
-}

diagonalLeftRight :: Integer -> Picture
diagonalLeftRight n
  | n <= 1        = black
  | otherwise     = ((diagonalLeftRight (m)) `beside` (whiteColumn (m)))
                    `above` (whiteRow (m) `beside` black)
  where m = (n-1)

{- 4.28
 - Give a haskell function which takes an integer n and returns an n by n white
 - square with a diagonal black line from top right to bottom left as in:
 - ___#
 - __#_
 - _#__
 - #___
-}

-- Without using diagonalLeftRight
diagonalRightLeft :: Integer -> Picture
diagonalRightLeft n
  | n <= 1        = black
  | otherwise     = (whiteRow (m) `beside` black) `above`
                    (diagonalRightLeft (m) `beside` whiteColumn (m))
  where m = (n-1)

{- 4.29
 - Give a haskell function which takes an integer n and returns an n by n white
 - square with both diagonals coloured black as in:
 - #__#
 - _##_
 - _##_
 - #__#
-}

diagonalBoth :: Integer -> Picture
diagonalBoth n
  | n <= 1         = black
  | n == 2         = (black `beside` black) `above` (black `beside` black)
  | otherwise      = borderTopBottom `above` (borderSide `beside` diagonalBoth (m)
                     `beside` borderSide) `above` borderTopBottom
  where m = (n-2)
        borderTopBottom = black `beside` whiteRow m `beside` black
        borderSide = whiteColumn m

{- 4.30
 - Can you give a direct recursive defintion of a function
     chessBoard :: Integer -> Picture
 - so that
     chessBoard n = ... chessBoard (n-1) ...
 - Hint: you might want to use some of the functions defined here, or variants of
         them, in writing your definition
-}

whiteBlackColumn :: Integer -> Picture
whiteBlackColumn n
  | n <= 1              = white
  | n `mod` 2 == 0      = whiteBlackColumn (n-1) `above` black
  | otherwise           = whiteBlackColumn (n-1) `above` white

blackWhiteColumn :: Integer -> Picture
blackWhiteColumn n = invertColour (whiteBlackColumn n)

chessBoard :: Integer -> Picture
chessBoard n
  | n <= 1              = white
  | n `mod` 2 == 0      = (chessBoard (n-1) `beside` blackWhiteColumn (n-1))
                          `above` blackWhite n
  | otherwise           = (chessBoard (n-1) `beside` whiteBlackColumn (n-1))
                          `above` whiteBlack n

{- 4.31
 - Give a recursive definition of a function to find the highest common factor of two
 - positive integers.
-}

isFactorOf :: Integer -> Integer -> Bool
isFactorOf x y = y `mod` x == 0

highestCommonFactor :: Integer -> Integer -> Integer -> Integer
highestCommonFactor x y z
  | (guess `isFactorOf` small) &&
    (guess `isFactorOf` large)  = guess
  | otherwise                   = highestCommonFactor x y (guess-1)

  where small = min x y
        large = max x y
        guess = min z small     -- guess should be smaller than smaller number
