{-# OPTIONS_GHC -Wall #-}

-- Exercise 1
-- Converts a positive Integer to a list of digits
toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

-- Converts a positive Integer to a list of digits in reverse
toDigitsRev :: Integer -> [Integer]
toDigitsRev x = [x `div` 10 ^ n `mod` 10
    | x > 0, let ns = [0..length(show x) - 1], n  <- ns]

-- Exercise 2
-- Doubles every other digit starting from the right
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = [xs !! (m - n) * isEvenToDigit n
    | let m = length xs, let ns = [m, m - 1..1], n <- ns]

-- Returns 2 if digit is even else returns 1
isEvenToDigit :: Int -> Integer
isEvenToDigit x
    | even x    = 2
    | otherwise = 1

-- Exercise 3
-- Calculates sum of all digits in a list
sumDigits :: [Integer] -> Integer
sumDigits xs = sum [sum(toDigits x) | x <- xs]
