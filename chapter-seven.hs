module ChapterSeven where

-- case practice
functionC x y = 
    case y of
        True -> x
        False -> y
    where y = 
            x > y
    
ifEvenAdd2 n =
    case even n of
        True -> n + 2
        False -> n

nums x = 
    case compare x 0 of
        LT -> -1
        GT -> 1
        EQ -> 0

-- artful dodgy
dodgy :: Num a => a -> a -> a
dodgy x y = x + y * 10

oneIsOne :: Num a => a -> a
oneIsOne = dodgy 1

oneIsTwo :: Num a => a -> a
oneIsTwo = (flip dodgy) 2

-- guard duty

avgGrade :: (Fractional a, Ord a)
        => a -> Char
avgGrade x
    | y >= 0.7 = 'C'
    | y >= 0.9 = 'A'
    | y >= 0.8 = 'B'
    | y >= 0.59 = 'D'
    | y < 0.59 = 'F'
    where y = x / 100

pal xs 
    | xs == reverse xs = True
    | otherwise         = False

numbers x
        | x < 0 = -1
        | x == 0 = 0
        | x > 0 = 1
