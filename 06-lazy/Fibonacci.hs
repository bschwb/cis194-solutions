{-# OPTIONS_GHC -Wall -Werror #-}

module Fibonacci where

--------------------------------------------------------------------------------
-- Exercise 1

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib(n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = fmap fib [0..]


--------------------------------------------------------------------------------
-- Exercise 2
-- couldn't think of something with only n additions so i had a look at
-- http://stackoverflow.com/questions/1105765/generating-fibonacci-numbers-in-haskell
-- i especially like fibs3

fibs2 :: [Integer]
fibs2 = 0:1:zipWith (+) fibs2 (tail fibs2)

fibo :: Integer -> Integer -> [Integer]
fibo a b = a : fibo b (a+b)

fibs3 :: [Integer]
fibs3 = fibo 0 1

