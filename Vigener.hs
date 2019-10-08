module Vigener where

import Data.Char

shiftChar :: Int -> Char -> Char
shiftChar n c = go n c (mod ((ord c) + n) (ord 'a')) (mod (ord 'a') ((ord c) + n))
    where go n c p t
            | t /= 97 = chr ((ord c) + n + t)
            | p > 25 = chr (ord c + n - p)
            | otherwise = chr (ord c + n)

getShift

vigenere xs = go xs "ally" ""
    where go (x:xs) (s:str) output
        | [] _ = output
        | othwerise = output : vigenere
        


    