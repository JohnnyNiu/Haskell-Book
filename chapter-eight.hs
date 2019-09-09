module ChapterEight where

import Data.List (intersperse)

addToN :: (Eq a, Num a) => a -> a
addToN n = go n 0
    where go n sum
            | n == 0 = sum
            | otherwise = go (n - 1) (sum + n)

addToMultiply :: (Integral a) => a -> a -> a
addToMultiply x y = go x y 0
    where go x y prod
            | y == 0 = prod
            | otherwise = go x (y - 1) (prod + x)

data DividedResult = Result Integer | DividedByZero deriving Show

dividedBy ::  Integer -> Integer -> (DividedResult, Integer)
dividedBy num denom = go num denom 0
    where go n d count
            | d == 0 = (DividedByZero, 0)
            | n < 0 = go (negate n) d count
            | d < 0 = go n (negate d) count
            | n < d = (Result (count
                            * (if num < 0 then (-1) else 1)
                            * (if denom < 0 then (-1) else 1)
                            ) , n) 
            | otherwise = go (n - d) d (count + 1)

mc91 n = if (n > 100)
            then n - 10
            else mc91 (mc91 (n + 11))


digitToWord :: Int -> String
digitToWord n = concat (intersperse "-" (map wordNumber (digits n)))

digits :: Int -> [Int]
digits n = go n []
    where go n output
            | div n 10 == 0 = mod n 10 : output
            | otherwise = go (div n 10) (mod n 10 : output)
            
wordNumber :: Int -> String
wordNumber 0 = "zero"
wordNumber 1 = "one"
wordNumber 2 = "two"
wordNumber 3 = "three"
wordNumber 4 = "four"
wordNumber 5 = "five"
wordNumber 6 = "six"
wordNumber 7 = "seven"
wordNumber 8 = "eight"
wordNumber 9 = "nine"