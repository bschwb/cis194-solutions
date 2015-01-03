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
hanoi 1 src goal _ = [(src, goal)]
hanoi n src goal tmp = hanoi (n-1) src tmp goal ++ hanoi 1 src goal tmp
  ++ hanoi (n-1) tmp goal src

