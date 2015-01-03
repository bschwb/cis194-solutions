{-# OPTIONS_GHC -Wall #-}

module Hanoi where

type Peg = String
type Move = (Peg, Peg)

-- Return list of moves to move n discs from the first Peg to the second.
-- hanoi 2 "a" "b" "c" == [("a","c"), ("a","b"), ("c","b")]
-- 1. move n-1 discs from a to c using b as temporary storage
-- 2. move the top disc from a to b
-- 3. move n-1 discs from c to b using a as temporary storage
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n src goal tmp
  | n <= 0    = []
  | n == 1    = [(src, goal)]
  | otherwise = hanoi (n-1) src tmp goal ++ hanoi 1 src goal tmp
                ++ hanoi (n-1) tmp goal src

-- Same principal but with 4 pegs this time
-- solution for 15 discs with version 1 should be 2^15-1 = 32767
-- with four pegs 129 moves
-- length(hanoi 15 "a" "b" "c") = 32767
-- length(hanoi 15 "a" "b" "c" "d") = 129
hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 n src goal tmp1 tmp2
  | n <= 0    = []
  | n == 1    = [(src, goal)]
  {-| n == 2    = [(src, tmp1), (src, goal), (tmp1, goal)]-}
  | n == 3    = [(src, tmp1), (src, tmp2), (src, goal), (tmp2, goal), (tmp1, goal)]
  | otherwise = hanoi4 (n-1) src tmp1 goal tmp2 ++ hanoi4 1 src goal tmp1 tmp2
                ++ hanoi4 (n-1) tmp1 goal src tmp2

