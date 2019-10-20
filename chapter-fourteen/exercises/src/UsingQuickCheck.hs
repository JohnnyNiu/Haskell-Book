module UsingQuickCheck where

import Test.QuickCheck
import Data.List (sort)

-- exercise one

half :: Fractional a => a -> a
half x = x / 2

halfIdentity :: Fractional a => a -> a
halfIdentity = (*2) . half

halfGen :: Gen Float
halfGen = arbitrary `suchThat` (/=0)

prop_halfEqual :: Property
prop_halfEqual = forAll halfGen (\x -> x == halfIdentity x)

halfQc :: IO ()
halfQc = do
    quickCheck prop_halfEqual

-- exercise two

listGen :: Gen [Int]
listGen = arbitrary

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs = snd $ foldr go (Nothing, True) xs
    where 
        go _ status@(_,False) = status
        go y (Nothing, t) = (Just y, t)
        go y (Just x, t) = (Just y, x >= y)

prop_listSorted :: Property 
prop_listSorted = forAll listGen (\x -> (listOrdered (sort x)) == True)

listQc :: IO ()
listQc = quickCheck prop_listSorted

-- exercise three

plusAssociative :: Integer -> Integer -> Integer -> Bool
plusAssociative x y z = x + (y + z) == (x + y) + z

plusCommutative :: Integer -> Integer -> Bool
plusCommutative x y = x + y == y + x

plusQc :: IO ()
plusQc = do
    quickCheck plusAssociative
    quickCheck plusCommutative

-- exercise four

multAssociative :: Integer -> Integer -> Integer -> Bool
multAssociative x y z = x * (y * z) == (x * y) * z

multCommutative :: Integer -> Integer -> Bool
multCommutative x y = x * y == y * x

multQc :: IO ()
multQc = do
    quickCheck plusAssociative
    quickCheck plusCommutative

-- exercise five

divGen :: Gen Integer
divGen = arbitrary `suchThat` (/=0)

quotRemProof :: Integer -> Integer -> Bool
quotRemProof x y = (quot x y) * y + (rem x y) == x

divModProof :: Integer -> Integer -> Bool
divModProof x y = (div x y) * y + (mod x y) == x

prop_quotRem = forAll divGen (\x -> \y -> quotRemProof x y == True)

prop_divMod = forAll divGen (\x -> \y -> divModProof x y == True)

-- not working, still tried to divide by zero

divQc :: IO ()
divQc = do
    quickCheck prop_divMod
    quickCheck prop_quotRem


-- exercise six

powerAsso :: Integer -> Integer -> Integer -> Bool
powerAsso x y z = x ^ (y ^ z) == (x ^ y) ^ z

powerCom :: Integer -> Integer -> Bool
powerCom x y = x ^ y == y ^ x

-- test reveals exponentiation is neither associative nor commutative

powerQc :: IO ()
powerQc = do
    quickCheck powerAsso
    quickCheck powerCom

-- exercise seven

prop_reverse :: [Int] -> Bool
prop_reverse xs = reverse (reverse xs) == xs

listReverseQc :: IO ()
listReverseQc = quickCheck prop_reverse

-- exercise eight

-- exercise nine

-- exercise ten

-- fails with empty list

f :: Int -> [Integer] -> Bool
f n xs = length (take n xs) == n

fQc :: IO ()
fQc = quickCheck f

-- exercise eleven

g :: String -> Bool
g x = (read (show x)) == x

gQc :: IO ()
gQc = quickCheck g