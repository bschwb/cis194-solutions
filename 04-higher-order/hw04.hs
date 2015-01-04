{-# OPTIONS_GHC -Wall #-}

module Homework where

import Data.List
-------------------------------------------------------------------------------
-- Exercise 4

-- Return all odd prime numbers up to 2 * @n@ + 2
sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map ((+1) . (*2)) $ [1..n] \\ sieve
  where sieve = map (\(i, j) -> i + j + 2*i*j)
                . filter (\(i, j) -> i + j + 2*i*j <= n)
                $ cartProd [1..n] [1..n]

-- Return all possible pairs
cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

-------------------------------------------------------------------------------
-- Exercise 3

-- Returns True if and only if there are an odd number of True values contained
-- in the input line. It does not matter how many False values the input list
-- contains.
-- > xor [False, True, False] == True
-- > xor [False, True, False, False, True] == False
xor :: [Bool] -> Bool
xor = foldr xor2 False

xor2 :: Bool -> Bool -> Bool
xor2 a b = (a || b) && not (a && b)

-- map as a fold
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x y -> f x : y) []

-- foldl with foldr
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f x = foldr (flip f) x . reverse

-------------------------------------------------------------------------------
-- Exercise 2

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

-- should produce a balanced Tree using @foldr@
foldTree :: Eq a => [a] -> Tree a
foldTree xs = foldr (balancedInsert start) Leaf xs
  where start = floor (logBase 2 $ fromIntegral(length xs)::Double)

balancedInsert :: Int -> a -> Tree a -> Tree a
balancedInsert _ _ _ = Leaf
{-balancedInsert _ x (Node n left y right)-}
          {-| right == Leaf = Node n left y (Node (n-1) Leaf x Leaf)-}
          {-| otherwise = Node n (Node (n-1) Leaf x Leaf) y right-}
{-balancedInsert start x _ = Node  Leaf x Leaf-}

-------------------------------------------------------------------------------
-- Exercise 1

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x    = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (subtract 2) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n    = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum
      . filter even
      . takeWhile (/= 1)
      . iterate (\n -> if even n then n `div` 2 else 3*n+1)

