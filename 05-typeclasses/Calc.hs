{-# OPTIONS_GHC -Wall #-}

module Calc where

import ExprT
import Parser

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

reify :: ExprT -> ExprT
reify = id

-- > eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) == 20
eval :: ExprT -> Integer
eval (Lit i) = i
eval (Add a b) = eval a + eval b
eval (Mul a b) = eval a * eval b

-- evaluates arithmetic expressions given as a String,
-- producing Nothing for inputs which are not well-formed expressions,
-- and Just n for well-formed inputs that evaluate to n
evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp Lit Add Mul


