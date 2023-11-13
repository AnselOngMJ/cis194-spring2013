{-# OPTIONS_GHC -Wall #-}

-- Exercise 1
-- Converts a positive Integer to a list of digits
toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

-- Converts a positive Integer to a list of digits in reverse
toDigitsRev :: Integer -> [Integer]
toDigitsRev n = [n `div` 10 ^ x `mod` 10
    | n > 0, let xs = [0..length(show n) - 1], x  <- xs]
