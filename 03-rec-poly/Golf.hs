{-# OPTIONS_GHC -Wall #-}

module Golf where

-- First list is the input itself, second list contains only every second
-- element of the input list, etc.
-- > skips "ABCD"        == ["ABCD", "BD", "C", "D"]
-- > skips "hello!"      == ["hello!", "el!", "l", "o", "!"]
-- > skips [1]           == [[1]]
-- > skips [True, False] == [[True,False], [False]]
-- > skips []            == []
skips :: [a] -> [[a]]
-- As mentioned in the specification for exercise 1, the output list is of the
-- same length as the input list.
skips lst = [each i lst | i <- [1..length lst]]

-- Returns each @n@-th element of the list.
-- > each 2 [1, 2, 3, 4] = [2, 4]
-- > each 2 [] = []
each :: Int -> [a] -> [a]
-- Using list comprehension. Index list returns exactly the indices of every
-- n-th element. Is safe because when the @lst@ is empty then the index list is
-- also empty. And indices go from 0 to (length lst) - 1, so also we don't
-- index anything to big.
each n lst = [lst !! i | i <- [n-1, n-1+n..length lst - 1]]

-- finds all local maxima in input list and returns them in order
-- > localMaxima [2,9,5,6,1] == [9,6]
-- > localMaxima [2,3,4,1,5] == [4]
-- > localMaxima [1,2,3,4,5] == []
localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:zs)
  | x < y && y > z = y : localMaxima (y:z:zs)
  | otherwise      = localMaxima (y:z:zs)
localMaxima _ = []

-- not needed anymore
-- extracts @k@ elements at position @n@
-- > extract 2 3 [1..10] == [3,4]
extract :: Int -> Int -> [Integer] -> [Integer]
extract k n = take k . drop (n-1)

-- takes a input list of Integers in [0,9] and outputs a vertical histogram
-- showing how many of each number were in the input list.
-- > histogram [1,1,1,5] ==
-- >
-- >  *
-- >  *
-- >  * *
-- > ==========
-- > 0123456789
--
-- use putStr (histogram [3,5]) to show this output in ghci
histogram :: [Integer] -> String
histogram xs = unlines (map (line c) [m+1,m..1]) ++ "==========\n0123456789\n"
  where c = count xs
        m = maximum c

-- returns one * line from the above function
line :: [Int] -> Int -> String
line xs n = [if i >= n then '*' else ' ' | i <- xs]

-- counts occurence of numbers in [0..9] in the input list.
count :: [Integer] -> [Int]
count xs = map (\n -> length $ filter (== n) xs) [0..9]

