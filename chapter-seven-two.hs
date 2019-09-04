module ChapterSevenTwo where

-- let's write code
tensDigit :: Integral a => a -> (a, a)
tensDigit x = x `divMod` 10

hunsD x = x `divMod` 100

foldBool :: a -> a -> Bool -> a
foldBool x y b = 
        case b of
            True -> x
            False -> y

foldBool2 :: a -> a -> Bool -> a
foldBool2 x y b 
        | b = x
        | otherwise = y

g :: (a -> b) -> (a, c) -> (b, c)
g f (a, b) = (f a, b)  

-- roundTrip :: (Show a, Read a) => a -> a
-- roundTrip a = read (show a)

-- roundTrip :: (Show a, Read a) => a -> a
-- roundTrip = read . show

roundTrip :: (Show a, Read b) => a -> b
roundTrip = read . show

main = do
    print ((roundTrip 4) :: Int)
    print (id 4)

