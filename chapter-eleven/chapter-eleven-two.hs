module ChapterElevenTwo where

import Data.Char
import Data.List

isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] _ = False
isSubseqOf _ [] = False
isSubSeqOf xs ys@(_:y) 
    | xs == take (length xs) ys = True
    | otherwise = isSubSeqOf xs y

capitalizeWords :: String -> [(String, String)]
capitalizeWords string = map (\x -> convert x) (words string)
    where convert xs@(x:y) = (xs, toUpper x : y)

capitalizeWord :: String -> String
capitalizeWord (x:xs) = (toUpper x) : xs

-- capitalizeParagraph :: String -> String
-- capitalizeParagraph str = go (words str) 0 []
--     where 
--         getWord w p = if p == 0 then capitalizeWord w else w
--         nextPos w p = if elem '.' w then 0 else p + 1


data DaPhone = DaPhone [(Char, String)]
    
myPhone = DaPhone [('1', ""), ('2', "ABC"), ('3', "DEF"),
                    ('4', "GHI"), ('5', "JKL"), ('6', "MNO"),
                    ('7', "PQRS"), ('8', "TUV"), ('9', "WXYZ"),
                    ('*', "^"), ('0', "* "), ('#', ".,")]

convo :: [String]
convo =
        ["Wanna play 20 questions",
            "Ya",
            "U 1st haha",
            "Lol ok. Have u ever tasted alcohol",
            "Lol ya",
            "Wow ur cool haha. Ur turn",
            "Ok. Do u think I am pretty Lol",
            "Lol ya",
            "Just making sure rofl ur turn"]

type Digit = Char
validButtons :: [Digit]
validButtons = ['1', '2', '3', '4', '5', '6', '7', '8', '9', '*', '0', '#'] 

type Presses = Int

-- getIndex :: (Digit, String) -> Char -> Maybe Presses
-- getIndex (d, letters) c
--   | c == d && not (isDigit c) = Just 0
--   | c == d && isDigit c = Just (length letters)
--   | otherwise = elemIndex c letters

-- reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
-- reverseTaps (DaPhone p) c
--     | isSpace c = [('0', 1)]
--     | isUpper c = ('*', 1) : go p (toLower c)
--     | otherwise = go p c
--     where go [] _ = []
--           go (x : xs) c' = case getIndex x c' of
--             Just i -> [(fst x, i + 1)]
--             Nothing -> go xs c'
  

data Expr = Lit Integer | Add Expr Expr deriving Show

eval :: Expr -> Integer
eval (Lit n) = n
eval (Add x y) = (eval x) + (eval y)

printExpr :: Expr -> String
printExpr (Lit n) = show n
printExpr (Add x y) = (printExpr x) ++ " + " ++ (printExpr y)