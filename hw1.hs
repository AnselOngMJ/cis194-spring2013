{-# OPTIONS_GHC -Wall #-}

-- Exercise 1
-- Converts a positive Integer to a list of digits
toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

-- Converts a positive Integer to a list of digits in reverse
toDigitsRev :: Integer -> [Integer]
toDigitsRev n = [n `div` 10 ^ x `mod` 10
    | n > 0, let xs = [0..length(show n) - 1], x  <- xs]

-- Exercise 2
-- Doubles every other digit starting from the right
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther ns = [ns !! (m - x) * isEvenToDigit x
    | let m = length ns, let xs = [m, m - 1..1], x <- xs]

-- Returns 2 if digit is even else returns 1
isEvenToDigit :: Int -> Integer
isEvenToDigit n
    | even n    = 2
    | otherwise = 1
