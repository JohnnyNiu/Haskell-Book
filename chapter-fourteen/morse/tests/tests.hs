module Main where

import qualified Data.Map as M
import Morse
import Test.QuickCheck

-- ggenerators to ensure random values QuickCheck uses are sensible for the program

allowedChars :: [Char]
allowedChars = M.keys letterToMorse

allowedMorse :: [Morse]
allowedMorse = M.elems letterToMorse

charGen :: Gen Char
charGen = elements allowedChars

morseGen :: Gen Morse
morseGen = elements allowedMorse

-- now property to be checked
-- want to check that we can convert something to morse and back without it changing

prop_thereAndBackAgain :: Property
prop_thereAndBackAgain =
    forAll charGen
    (\c -> ((charToMorse c) >>= morseToChar) == Just c)

main :: IO ()
main = quickCheck prop_thereAndBackAgain