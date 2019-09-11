module ChapterNine where

import Data.Char
-- enumFromTo exercises

eftBool :: Bool -> Bool -> [Bool]
eftBool False False = [False]
eftBool False True = [False, True]
eftBool True False = []
eftBool True True = [True]

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd LT LT = [LT]
eftOrd LT GT = [LT, GT]
eftOrd LT EQ = [LT, EQ]
eftOrd GT GT = [GT]
eftOrd GT LT = []
eftOrd GT EQ = []
eftOrd EQ EQ = [EQ]
eftOrd EQ LT = []
eftOrd EQ GT = [EQ, GT]

eftInt :: Int -> Int -> [Int]
eftInt x y 
    | x < y = [x, y] -- should be x : eftInt (x + 1) y
    | y > x = []
    | y == x = [x]

eftChar :: Char -> Char -> [Char]
eftChar x y 
    | x > y = []
    | x == y = [x]
    | otherwise = x : eftChar (succ x) y 

-- thy fearful symmetry

myWords x = go (dropWhile (== ' ' ) x) []
    where go str list
            | length str == 0 = list
            | otherwise = go (dropWhile (== ' ') (dropWhile (/= ' ') str)) (list ++ [(takeWhile (/= ' ') str)])

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful symmetry?"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

myLines :: String -> [String]
myLines x = go  (dropWhile (== '\n') x) []
    where go str list
            | length str == 0 = list
            | otherwise = go (dropWhile (== '\n') (dropWhile (/= '\n') str)) (list ++ [(takeWhile (/= '\n') str)])

dropBy :: String -> Char -> [String]
dropBy x y = go y x []
    where go char str list
            | length str == 0 = list
            | otherwise = go char (dropWhile (== char) (dropWhile (/= char) str)) (list ++ [(takeWhile (/= char) str)])


-- zipping exercises

myZip :: [a] -> [b] -> [(a, b)]
myZip [] _ = []
myZip _ [] = []
myZip (x:xs) (y:ys) = (x, y) : myZip xs ys

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ [] _ = []
myZipWith _ _ [] = []
myZipWith f (x:xs) (y:ys) = (f x y ) : myZipWith f xs ys

myZip2 :: [a] -> [b] -> [(a, b)]
myZip2 (x:xs) (y:ys) = myZipWith (,) xs ys


-- chapter exercises

-- data.char

f :: String
f = filter (\x -> isUpper x) "HbEfLrLxO,"

capFirst :: String -> String
capFirst [] = []
capFirst (x:xs) = toUpper x : xs

capFirst2 :: String -> String
capFirst2 [] = []
capFirst2 (x:xs) = toUpper x : capFirst2 xs

capFirst3 :: String -> Char
capFirst3 [] = ' '
capFirst3 xs = toUpper (head xs)

capFirst4 :: String -> Char
capFirst4 = toUpper . head 

-- standard functions

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = 
    if x == True
    then True
    else myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False 
myAny f (x:xs) =
    if (f x == True)
        then True
        else myAny f xs

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem x (y:ys) = 
    if x == y
        then True
        else myElem x ys

myElem2 :: Eq a => a -> [a] -> Bool
myElem2 _ [] = False
myElem2 x ys = any (x ==) ys

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (x:xs) = f x ++ squishMap f xs

squishAgain :: [[a]] -> [a]
squishAgain [] = []
squishAgain (x:xs) = squishMap id xs

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x:y:[]) = if f x y == GT then x else y
myMaximumBy f (x:y:xs) 
            | f x y == GT = myMaximumBy f (x:xs)
            | otherwise = myMaximumBy f (y:xs)

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (x:y:[]) = if f x y == LT then x else y
myMinimumBy f (x:y:xs) 
            | f x y == LT = myMaximumBy f (x:xs)
            | otherwise = myMaximumBy f (y:xs)

myMaximum :: (Ord a) => [a] -> a
myMaximum xs = myMaximumBy compare xs

myMinimum :: (Ord a) => [a] -> a
myMinimum xs = myMinimumBy compare xs

