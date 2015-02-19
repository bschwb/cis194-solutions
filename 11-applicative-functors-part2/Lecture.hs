{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lecture where

import Control.Applicative hiding ((*>))

(.+) :: (Num a, Applicative f) => f a -> f a -> f a
(.+) = liftA2 (+)
(.*) :: (Num a, Applicative f) => f a -> f a -> f a
(.*) = liftA2 (*)

newtype ZipList' a = ZipList' { getZipList' :: [a] }
  deriving (Eq, Show, Functor)

instance Applicative ZipList' where
  pure = ZipList' . repeat
  ZipList' fs <*> ZipList' xs = ZipList' (zipWith ($) fs xs)

pair :: Applicative f => f a -> f b -> f (a, b)
pair = liftA2 (,)

(*>) :: Applicative f => f a -> f b -> f b
{-(*>) = snd <$> pair-}
{-a *> b = snd <$> pair a b-}
-- or as in the Control.Applicative implementation
(*>) = liftA2 (const id)
-- or my thoughts when looking at the types of "const id"
{-(*>) = liftA2 (flip const)-}

-- following implementations I found at
-- github.com/xificurc/cis194/hw11/Lecture.hs
mapA :: Applicative f => (a -> f b) -> ([a] -> f [b])
mapA f = foldr (liftA2 (:) . f ) $ pure []

-- to test mapA
safeDivision :: Int -> Int -> Maybe Int
safeDivision _ 0 = Nothing
safeDivision a b = Just (a `div` b)

sequenceA :: Applicative f => [f a] -> f [a]
sequenceA = foldr (liftA2 (:)) (pure [])

replicateA :: Applicative f => Int -> f a -> f [a]
replicateA = liftA . replicate

