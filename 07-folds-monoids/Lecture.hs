{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lecture where

import qualified Data.Monoid as Mon

data Tree a = Empty
            | Node (Tree a) a (Tree a)
  deriving (Show, Eq)

leaf :: a -> Tree a
leaf x = Node Empty x Empty

treeSize :: Tree a -> Integer
treeSize Empty        = 0
treeSize (Node l _ r) = 1 + treeSize l + treeSize r

treeSum :: Tree Integer -> Integer
treeSum Empty        = 0
treeSum (Node l x r) = x + treeSum l + treeSum r

treeDepth :: Tree a -> Integer
treeDepth Empty        = 0
treeDepth (Node l _ r) = 1 + max (treeDepth l) (treeDepth r)

flatten :: Tree a -> [a]
flatten Empty        = []
flatten (Node l x r) = flatten l ++ [x] ++ flatten r

treeFold :: b -> (b -> a -> b -> b) -> Tree a -> b
treeFold e _ Empty = e
treeFold e f (Node l x r) = f (treeFold e f l) x (treeFold e f r)

treeSize' :: Tree a -> Integer
treeSize' = treeFold 0 (\l _ r -> 1 + l + r)

treeSum' :: Tree Integer -> Integer
treeSum' = treeFold 0 (\l x r -> x + l + r)

-- Monoids
{-instance Monoid [a] where-}
  {-mempty = []-}
  {-mappend = (++)-}

newtype Sum a = Sum a
  deriving (Eq, Ord, Num, Show)

getSum :: Sum a -> a
getSum (Sum a) = a

instance Num a => Mon.Monoid (Sum a) where
  mempty = Sum 0
  mappend = (+)

newtype Product a = Product a
  deriving (Eq, Ord, Num, Show)

getProduct :: Product a -> a
getProduct (Product a) = a

instance Num a => Mon.Monoid (Product a) where
  mempty = Product 1
  mappend = (*)

instance Mon.Monoid Bool where
  mempty = False
  mappend = (||)

