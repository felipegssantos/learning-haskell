{-# OPTIONS_GHC -Wall #-}

-- Take a number and output its digits reversed in a list
toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0 = []
  | otherwise = (n `mod` 10) : (toDigitsRev (n `div` 10)) 

-- Take a number and output its digits in a list
toDigits :: Integer -> [Integer]
toDigits n = reverse (toDigitsRev n)

-- Double 1st, 3rd, ... numbers in a list
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther (x:y:zs) = x : 2*y : (doubleEveryOther zs)

-- sumDigits of a list of integers
sumDigits :: [Integer] -> Integer
sumDigits (x:xs)
  | x < 0 = sumDigits xs
  | x < 10 = x + sumDigits xs
  | otherwise = sumDigits (toDigits x) + sumDigits xs 
sumDigits [] = 0

-- Validate a credit card number
validate :: Integer -> Bool
validate n = sumDigits (doubleEveryOther (toDigitsRev n)) `mod` 10 == 0

