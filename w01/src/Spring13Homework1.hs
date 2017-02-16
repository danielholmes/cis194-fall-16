module Spring13Homework1 (
    toDigits, toDigitsRev, doubleEveryOther, sumDigits, validate
) where

toDigits :: Integer -> [Integer]
toDigits n = reverse (toDigitsRev n)

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
    | n <= 0 = []
    | otherwise = n `mod` 10 : (toDigitsRev ((fromIntegral n) `div` 10))

-- Inefficient. TODO: Surely a better way?
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = reverse (doubleEveryOtherRev (reverse xs))

doubleEveryOtherRev :: [Integer] -> [Integer]
doubleEveryOtherRev [] = []
doubleEveryOtherRev [x] = [x]
doubleEveryOtherRev (x:y:xs) = x : (y*2) : doubleEveryOtherRev xs

sumDigits :: [Integer] -> Integer
sumDigits n = sum (map sum (map toDigits n))

validate :: Integer -> Bool
validate n = (sumDigits (doubleEveryOther (toDigits n))) `mod` 10 == 0