{-# OPTIONS_GHC -Wall #-}

module Lecture where

hailstoneLen :: Integer -> Integer
hailstoneLen n = intListLength (hailstone n) - 1

--
sumEveryTwo :: [Integer] -> [Integer]
sumEveryTwo [] = []
sumEveryTwo [x] = [x]
sumEveryTwo (x:y:xs) = (x+y) : sumEveryTwo xs

-- Returns length of list of integers
intListLength :: [Integer] -> Integer
intListLength [] = 0
intListLength (_:xs) = 1 + intListLength xs

hailstone :: Integer -> [Integer]
hailstone 1 = [1]
hailstone n = n : hailstone (n-1)

isEven :: Integer -> Bool
isEven n = n `mod` 2 == 0

foo :: Integer -> Integer
foo 0 = 16
foo 1
  | "Haskell" > "C++" = 3
  | otherwise         = 4
foo n
  | n < 0 = 0
  | n `mod` 17 == 2 = -43
  | otherwise = n + 3

