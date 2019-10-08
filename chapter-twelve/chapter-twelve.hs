module ChapterTwelve where

-- string processing

-- Exercise One

notThe :: String -> Maybe String
notThe str  
    | str == "the" = Nothing
    | otherwise = Just str

replaceThe :: String -> String
replaceThe str = unwords $ map f $ map notThe $ words str
    where 
            f :: Maybe String -> String
            f (Just x) = x
            f _ = "a"

-- Exercise Three

isVowel :: Char -> Bool
isVowel 'a' = True
isVowel 'e' = True
isVowel 'i' = True
isVowel 'o' = True
isVowel 'u' = True
isVowel _ = False

getVowels :: String -> String
getVowels xs = [ x | x <- xs , isVowel x ]

countVowels :: String -> Int
countVowels str = length $ getVowels str


-- Validate the word

newtype Word' = Word' String deriving (Eq, Show)

consonants :: String -> String
consonants xs = [ x | x <- xs , not $ isVowel x ]

countConsonants :: String -> Int
countConsonants str = length $ consonants str

mkWord :: String -> Maybe Word'
mkWord str = case countConsonants str > countVowels str of 
    True -> Just $ Word' str
    False -> Nothing

-- It's only natural

data Nat = Zero | Succ Nat deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ n) = 1 + natToInteger n

integerToNat :: Integer -> Nat
integerToNat x = undefined

-- Small library for Maybe

-- 1
isJust :: Maybe a -> Bool
isJust (Just a) = True
isJust _ = False

-- 2
mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee z f (Just x) = f x
mayybee z f (Nothing) = z

-- 4
listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:xs) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just a) = a : []

-- 5
removeJust :: Maybe a -> a
removeJust (Just a) = a

catMaybes :: [Maybe a] -> [a]
catMaybes xs = map removeJust . filter isJust $ xs


-- Small library for either

-- 1
getLeft :: Either a b -> [a] -> [a]
getLeft (Left a) xs = a : xs
getLeft _ xs = xs

lefts' :: [Either a b] -> [a]
lefts' xs = foldr getLeft [] xs

-- 2
getRight :: Either a b -> [b] -> [b]
getRight (Right b) xs = b : xs
getRight _ xs = xs

rights' :: [Either a b] -> [b]
rights' xs = foldr getRight [] xs

-- 3

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' xs = (,) (lefts' xs) (rights' xs)

-- 4

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Right b) = Just (f b)
eitherMaybe' f _ = Nothing

-- 5

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f g (Left a) = f a
either' f g (Right b) = g b

-- Unfolds