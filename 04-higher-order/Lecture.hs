{-# OPTIONS_GHC -Wall #-}

module Lecture where

schönfinkel :: ((a,b) -> c) -> a -> b -> c
schönfinkel f x y = f (x,y)

unschönfinkel :: (a -> b -> c) -> (a, b) -> c
unschönfinkel f (x,y) = f x y

