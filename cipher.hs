module Cipher where

import Data.Char

caesar :: Int -> String -> String
caesar n str = map (\x -> shiftChar n x ) str

shiftChar :: Int -> Char -> Char
shiftChar n c = go n c (mod ((ord c) + n) (ord 'a')) (mod (ord 'a') ((ord c) + n))
    where go n c p t
            | t /= 97 = chr ((ord c) + n + t)
            | p > 25 = chr (ord c + n - p)
            | otherwise = chr (ord c + n)

uncaesar :: Int -> String -> String
uncaesar n (x:xs)
        | xs == [] = shiftChar ((-1) * n) x : xs
        | otherwise = shiftChar ((-1) * n) x : uncaesar n xs
