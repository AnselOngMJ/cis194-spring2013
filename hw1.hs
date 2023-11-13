{-# OPTIONS_GHC -Wall #-}

-- Exercise 1
-- Converts a positive Integer to a list of digits
toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

-- Converts a positive Integer to a list of digits in reverse
toDigitsRev :: Integer -> [Integer]
toDigitsRev x = [x `div` 10 ^ n `mod` 10
    | x > 0, let ns = [0..length (show x) - 1], n  <- ns]

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
sumDigits xs = sum [sum (toDigits x) | x <- xs]

-- Exercise 4
-- Indicates whether an Integer is a valid credit card number
validate :: Integer -> Bool
validate x
    | sumDigits (doubleEveryOther (toDigits x)) `mod` 10 == 0 = True
    | otherwise                                               = False

-- Exercise 5
-- Returns a list of moves to move discs from peg a to peg b
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi x a b c = hanoi (x - 1) a c b ++ [(a, b)] ++ hanoi (x - 1) c b a
