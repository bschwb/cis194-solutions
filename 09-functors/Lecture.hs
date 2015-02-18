{-# OPTIONS_GHC -Wall #-}

module Lecture where

main :: IO ()
main = putStrLn "Please enter a number: " >> (readLn >>= (\n -> putStrLn (show (n+1))))

