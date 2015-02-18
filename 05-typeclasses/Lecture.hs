{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE FlexibleInstances #-}

module Lecture where

instance Listable Int where
  -- toList :: Int -> [Int]
  toList x = [x]

instance Listable Bool where
  toList True = [1]
  toList False = [0]

instance Listable [Int] where
  toList = id

data Tree a = Empty | Node a (Tree a) (Tree a)

instance Listable (Tree Int) where
  toList Empty        = []
  toList (Node x l r) = toList l ++ [x] ++ toList r

sumL x = sum (toList x)

foo x y = sum (toList x) == sum (toList y) || x < y

instance (Listable a, Listable b) => Listable (a, b) where
  toList (x,y) = toList x ++ toList y

class Listable a where
  toList :: a -> [Int]

-------------------------------------------------------------------------------
data Foo = F Int | G Char

instance Eq Foo where
  (F i1) == (F i2) = i1 == i2
  (G c1) == (G c2) = c1 == c2
  _ == _ = False
  foo1 /= foo2 = foo1 /= foo2

data Foo' = F' Int | G' Char
  deriving (Eq, Ord, Show)

