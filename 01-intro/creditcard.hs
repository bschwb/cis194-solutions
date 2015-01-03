{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE ViewPatterns #-}

module CreditCard where

-- Return True for a valid credit card number
-- validate 4012888888881881 = True
-- validate 4012888888881882 = False
-- validate -4012888888881881 = False
validate :: Integer -> Bool
validate n
  | n > 0     = mod (sumDigits . doubleEveryOther . toDigits $ n) 10 == 0
  | otherwise = False

-- Sum over all digits of a list of Integers
-- sumDigits [16,7,12,5] = 1 + 6 + 7 + 1 + 2 + 5 = 22
sumDigits :: [Integer] -> Integer
sumDigits lst = sum $ map (sum . toDigits) lst
sumDigits = sum . map (sum . toDigits)

-- Doubles every other number beginning from the rights
-- doubleEveryOther [8,7,6,5] == [16,7,12,5]
-- doubleEveryOther [1,2,3] == [1,4,3]
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther (reverse -> (x:y:zs)) =
  doubleEveryOther (reverse zs) ++ [2*y, x]
doubleEveryOther _ = []

-- without viewpatterns
-- see github evansb/cis194-hw/hw1/hw1.hs
doubleEveryOtherV2 :: [Integer] -> [Integer]
doubleEveryOtherV2 = reverse . zipWith (*) oneTwo . reverse
  where oneTwo = 1 : 2 : oneTwo

-- Converts positive Integers to a list of their digits.
-- toDigits 1234 == [1,2,3,4]
-- toDigits 0 == []
-- toDigits (-17) == []
toDigits :: Integer -> [Integer]
toDigits n
  {-| n > 0   = reverse $ toDigitsRev n-}
  | n > 0     = toDigits (n `div` 10) ++ [n `mod` 10]
  | otherwise = []

-- Converts positive Integers to a reversed list of their digits.
-- toDigits 1234 == [4,3,2,1]
toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n > 0     = n `mod` 10 : toDigitsRev (n `div` 10)
  | otherwise = []

-- Return digit of Integer n at specified place exponent of 10
-- digitAtPlace 1234 4 = 1
-- digitAtPlace 1234 4 = 1
digitAtPlace :: Integer -> Integer -> Integer
digitAtPlace n expo
  | expo >= 0 = (n `div` (10^expo)) `mod` 10
  | otherwise = -1

