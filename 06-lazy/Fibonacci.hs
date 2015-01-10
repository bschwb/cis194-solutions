{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

{-# LANGUAGE FlexibleInstances #-}

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
-- i especially like fibs4

-- my idea which does not work
{-fibs2 :: [Integer]-}
{-fibs2 = fiba [1, 0]-}

{-fiba :: [Integer] -> [Integer]-}
{-fiba (a:b:ys) = fiba(a+b:a:b:ys)-}
{-fiba _ = []-}

fibs3 :: [Integer]
fibs3 = 0:1:zipWith (+) fibs3 (tail fibs3)

fibo :: Integer -> Integer -> [Integer]
fibo a b = a : fibo b (a+b)

fibs4 :: [Integer]
fibs4 = fibo 0 1

--------------------------------------------------------------------------------
-- Eyercise 3
data Stream a = Cons a (Stream a)

instance Show a => Show (Stream a) where
  show = show . take 20 . streamToList

streamToList :: Stream a -> [a]
streamToList (Cons y c) = y : streamToList c

--------------------------------------------------------------------------------
-- Eyercise 4

streamRepeat :: a -> Stream a
streamRepeat y = Cons y (streamRepeat y)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons y ys) = Cons (f y) (streamMap f ys)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f y = Cons y (streamFromSeed f (f y))

--------------------------------------------------------------------------------
-- Eyercise 5
nats :: Stream Integer
nats = streamFromSeed (+1) 0


interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons y ys) zs = Cons y (interleaveStreams zs ys)

-- > 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4
ruler :: Stream Integer
ruler = startRuler 0

-- > 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4
-- Eric D.Burgess - http://oeis.org/A001511
startRuler :: Integer -> Stream Integer
startRuler y = interleaveStreams (streamRepeat y) (startRuler (y+1))

--------------------------------------------------------------------------------
-- Exercise 6
x :: Stream Integer
x = Cons 0 (Cons 1 (streamRepeat 0))

instance Num (Stream Integer) where
    fromInteger n = Cons n (streamRepeat 0)
    negate (Cons y ys) = Cons (-y) (negate ys)
    -- | A*B = a0*b0+x*(a0*B' + A' * B)
    (+) (Cons y ys) (Cons z zs) = Cons (y+z) (ys + zs)
    (*) (Cons y ys) s@(Cons z zs) = Cons (y*z) (streamMap (*y) zs + (ys*s))

instance Fractional (Stream Integer) where
    (/) (Cons y ys) (Cons z zs) = q
        where q = Cons (y `div` z) (streamMap (`div` z) (ys - q * zs))

fibs10 :: Stream Integer
fibs10 = x / (1 - x - x * x)

