{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE TypeSynonymInstances #-}

module Lecture where
-- The bit about the Parser-Monad is now in AParser.hs

{-data Maybe a = Just a | Nothing-}
  {-deriving (Show, Eq)-}

{-instance Monad Maybe where-}
  {-return = Just-}
  {-Nothing >>= _ = Nothing-}
  {-Just x  >>= k = k x-}

check :: Int -> Maybe Int
check n | n < 10    = Just n
        | otherwise = Nothing

halve :: Int -> Maybe Int
halve n | even n    = Just $ n `div` 2
        | otherwise = Nothing

addOneOrTwo :: Int -> [Int]
addOneOrTwo x = [x+1, x+2]

{-instance Monad Parser where-}
  {-return = pure-}
  {-p >>= k = k a-}
    {-where -}

