module Exercises where

import Test.QuickCheck

-- Heavy Lifting
-- one
a = fmap (+1) $ read "[1]" :: [Int]

--two
b = (fmap.fmap) (++ "lol") (Just ["Hi, ", "Hello"])

--three
c = fmap (*2) (\x -> x - 2)

--four
d = fmap
    ((return '1' ++) . show)
    (\x -> [x, 1..3])

--five
-- doesn't compile
-- e :: IO Integer
-- e = let ioi = readIO "1" :: IO Integer
--         changed = read ("123"++) show ioi
--     in fmap (*3) changed

-- Instances of Func

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

functorIdentityInt :: [Int] -> Bool
functorIdentityInt = functorIdentity

functorComposeInt :: [Int] -> Bool
functorComposeInt =
  functorCompose (+1) (*2)

-- main :: IO ()
-- main = do
--     quickCheck functorIdentity1


--one
newtype Identity a = Identity a deriving (Eq, Show)

instance Functor (Identity) where
    fmap f (Identity a) = Identity (f a)

functorIdentity1 :: Identity Int -> Bool
functorIdentity1 x = functorIdentity x



--two
data Pair a = Pair a a deriving (Eq, Show)

instance Functor (Pair) where
    fmap f (Pair a b) = Pair (f a) (f b)

functorIdentity2 :: Pair Int -> Bool
functorIdentity2 = functorIdentity

-- three
data Two a b = Two a b

instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)

-- four
data Three a b c = Three a b c

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)

-- five
data Three' a b = Three' a b b

instance Functor (Three' a) where
    fmap f (Three' a b c) = Three' a (f b) (f c)

-- six
data Four a b c d = Four a b c d

instance Functor (Four a b c) where
    fmap f (Four a b c d) = Four a b c (f d)

-- seven
data Four' a b = Four' a a a b

instance Functor (Four' a) where
    fmap f (Four' a b c d) = Four' a b c (f d)


-- Possibly

data Possibly a = LolNope | Yeppers a deriving (Eq, Show)

instance Functor Possibly where
    fmap f LolNope = LolNope
    fmap f (Yeppers a) = Yeppers (f a)

-- Short exercise

data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
    fmap f (Second b) = Second (f b)