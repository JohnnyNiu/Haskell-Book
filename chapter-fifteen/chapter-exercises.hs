module ChapterExercises where

import Data.Semigroup
import Test.QuickCheck

-- Semigroup Exercises

-- One

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
    _ <> _ = Trivial

instance Arbitrary Trivial where
    arbitrary = return Trivial

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)
    
type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool
   


-- Two

newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
    (Identity a) <> (Identity b) = Identity (a <> b)

instance Arbitrary a => Arbitrary (Identity a)
    where arbitrary = do
            a <- arbitrary
            return (Identity a)

type IdAssoc = Identity String -> Identity String -> Identity String -> Bool

-- Three

data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
    (Two a b) <> (Two c d) = Two (a <> c) (b <> d)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b)
    where arbitrary = do
            a <- arbitrary
            b <- arbitrary
            return (Two a b)

type TwoAssoc = Two String String -> Two String String -> Two String String -> Bool


-- Four

data Three a b c = Three a b c deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
    (Three a b c) <> (Three a' b' c') = Three (a <> a') (b <> b') (c <> c')

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c)
    where arbitrary = do
            a <- arbitrary
            b <- arbitrary
            c <- arbitrary
            return (Three a b c)

type ThreeAssoc = Three String String String -> Three String String String -> Three String String String -> Bool

-- six

newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance (Semigroup BoolConj) where
    (BoolConj True) <> (BoolConj True) = BoolConj True
    _ <> _ = BoolConj False

instance (Arbitrary BoolConj) where
    arbitrary = do
        a <- arbitrary
        return (BoolConj a)

type BoolConjAssoc = BoolConj -> BoolConj  -> BoolConj -> Bool


-- seven

newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
    (BoolDisj True) <> _ = BoolDisj True
    _ <> _ = BoolDisj False

instance (Arbitrary BoolDisj) where
    arbitrary = do
        a <- arbitrary
        return (BoolDisj a)

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool



-- eight

data Or a b = Fst a | Snd b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Or a b) where
    (Snd a) <> _  = Snd a 
    _ <> (Snd b) = Snd b
    (Fst a) <> _ = Fst a

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
    arbitrary = do
            a <- arbitrary
            b <- arbitrary
            frequency [ 
                (1, return $ Fst a),
                (1, return $ Snd b)]

type OrAssoc = Or String String -> Or String String -> Or String String -> Bool

-- main :: IO ()
-- main = do
--     quickCheck (semigroupAssoc :: TrivAssoc) 
--     quickCheck (semigroupAssoc :: IdAssoc)
--     quickCheck (semigroupAssoc :: TwoAssoc)
--     quickCheck (semigroupAssoc :: ThreeAssoc)
--     quickCheck (semigroupAssoc :: BoolConjAssoc)
--     quickCheck (semigroupAssoc :: BoolDisjAssoc)
--     quickCheck (semigroupAssoc :: OrAssoc)

-- nine

-- ten

-- eleven

data Validation a b =
    Fail a | Succ b
    deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
    (Succ a) <> _ = Succ a
    _ <> (Succ b) = Succ b
    (Fail a) <> (Fail b) = Fail (a <> b)

main = do
    let failure :: String -> Validation String Int
        failure = Fail
        success :: Int -> Validation String Int
        success = Succ
    print $ success 1 <> failure "blah"
    print $ failure "woot" <> failure "blah"
    print $ success 1 <> success 2
    print $ failure "woot" <> success 2