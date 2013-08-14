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

-- Functions for exercises 4.27-29
whiteSquares :: Integer -> (Picture -> Picture -> Picture) -> Picture
whiteSquares n positionFn
  | n <= 1     = white
  | otherwise  = positionFn white (whiteSquares (n-1) positionFn)

whiteColumn :: Integer -> Picture
whiteColumn n = whiteSquares n above

whiteRow :: Integer -> Picture
whiteRow n = whiteSquares n beside

{- 4.27
 -
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
 -
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
 -
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
  | otherwise      = borderTop `above` (borderSide `beside` diagonalBoth (m)
                     `beside` borderSide) `above` borderBottom
  where m = (n-2)
        borderTop = borderBottom = black `beside` whiteRow x `beside` black
        borderSide = whiteColumn m
