{-# OPTIONS_GHC -Wall #-}

module Lecture where

data Tree = Leaf Char
          | Node Tree Int Tree
  deriving Show

data IntList = Empty | Cons Int IntList

intListProd :: IntList -> Int
intListProd Empty = 1
intListProd (Cons x y) = x * intListProd y

baz :: Person -> String
baz p@(Person n _ _) = "The name field of (" ++ show p ++ ") is " ++ n

getAge :: Person -> Int
getAge (Person _ a _) = a

data Person = Person String Int Thing
  deriving Show

failureToZero :: FailableDouble -> Double
failureToZero Failure = 0
failureToZero (OK d)  = d

saveDiv :: Double -> Double -> FailableDouble
saveDiv _ 0 = Failure
saveDiv x y = OK (x / y)

data FailableDouble = Failure
                    | OK Double
  deriving Show

data Thing = Shoe
           | Ship
           | SealingWax
           | Cabbage
           | King
  deriving Show

isSmall :: Thing -> Bool
isSmall Shoe       = True
isSmall Ship       = False
isSmall SealingWax = True
isSmall Cabbage    = True
isSmall King       = False

isSmall2 :: Thing -> Bool
isSmall2 Shoe = True
isSmall2 King = True
isSmall2 _    = False

