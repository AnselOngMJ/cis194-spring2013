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
-- Returns a list of moves to move discs from a to b with 3 pegs
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi x a b c = hanoi (x - 1) a c b ++ [(a, b)] ++ hanoi (x - 1) c b a

-- Exercise 6
-- Returns of a list of moves to move discs from a to b with 4 pegs
hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 0 _ _ _ _ = []
hanoi4 1 a b _ _ = [(a, b)]
hanoi4 x a b c d
  | x < 6     = hanoi4 (x - 2) a d b c ++ [(a, c), (a, b), (c, b)]
                ++ hanoi4 (x - 2) d b a c
  | otherwise = hanoi4 k a d b c ++ hanoi (x - k) a b c ++ hanoi4 k d b a c
  where 
    k = x - round (sqrt (2 * (fromIntegral x :: Double) + 1)) + 1
