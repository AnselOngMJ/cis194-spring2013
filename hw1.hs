{-# OPTIONS_GHC -Wall #-}

-- Exercise 1
toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev
toDigitsRev :: Integer -> [Integer]
toDigitsRev n = [n `div` 10 ^ x `mod` 10
    | n > 0, let xs = [0..length(show n) - 1], x  <- xs]
