{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module JoinList where

import Data.Monoid

import Buffer
import Editor
import Scrabble
import Sized

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

instance Buffer (JoinList (Score, Size) String) where
  toString             = unlines . jlToList

  fromString           = foldl (\jl str -> jl +++ scoreLine' str) Empty . lines
    where scoreLine' str = Single (scoreString str, 1) str

  line                 = indexJ

  replaceLine n str jl = takeJ n jl +++ fromString str +++ dropJ (n+1) jl

  numLines             = getSize . snd . tag

  value                = getScore . fst . tag

main = runEditor editor (fromString "test" :: (JoinList (Score, Size) String))

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) a b = Append (tag a <> tag b) a b

tag :: Monoid m => JoinList m a -> m
tag (Single m _) = m
tag (Append m _ _) = m
tag _ = mempty

-- | Finds the JoinList element at the specified index.
-- | (indexJ i jl) == (jlToList jl !!? i)
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ index (Single _ a)
  | index == 0 = Just a
  | otherwise  = Nothing
indexJ index (Append m l1 l2)
  | index < 0 || index > size0 = Nothing
  | index < size1              = indexJ index l1
  | otherwise                  = indexJ (index - size1) l2
    where size0 = getSize . size $ m
          size1 = getSize . size . tag $ l1
indexJ _ _ = Nothing

-- | Drops the first @n@ elements of @jl@
-- | jlToList (dropJ n jl) == drop n (jlToList jl)
dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ n l1@(Single _ _)
  | n <= 0 = l1
dropJ n l@(Append m l1 l2)
  | n >= size0 = Empty
  | n >= size1 = dropJ (n-size1) l2
  | n > 0 = dropJ n l1 +++ l2
  | otherwise  = l
    where size0 = getSize . size $ m
          size1 = getSize . size . tag $ l1
dropJ _ _ = Empty

scoreLine :: String -> JoinList Score String
scoreLine str = Single (scoreString str) str

-- | Returns the first @n@ elements of @jl@
-- | jlToList (takeJ n jl) == take n (jlToList jl)
takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ n l1@(Single _ _)
  | n > 0 = l1
takeJ n l@(Append m l1 l2)
  | n >= size0 = l
  | n >= size1 = l1 +++ takeJ (n-size1) l2
  | n > 0 = takeJ n l1
    where size0 = getSize . size $ m
          size1 = getSize . size . tag $ l1
takeJ _ _ = Empty

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

-- Just for testing. All identities should hold as descriped in the comments.
-- Example:
-- (indexJ index a) == (jlToList a !!? index) for all indices i
(!!?) :: [a] -> Int -> Maybe a
[]     !!? _         = Nothing
_      !!? i | i < 0 = Nothing
(x:xs) !!? 0         = Just x
(x:xs) !!? i         = xs !!? (i-1)


a = Append (Size 3)
      (Append (Size 2)
        (Single (Size 1) "hi")
        (Single (Size 1) "bye")
      )
     (Single (Size 1) "tschau")

b = Single (Size 1) "blub"

c = Append (Size 2)
      (Single (Size 1) "hi")
      (Single (Size 1) "bye")

