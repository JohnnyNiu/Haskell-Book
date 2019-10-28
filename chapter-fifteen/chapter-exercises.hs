module ChapterExercises where

import Data.Semigroup
import Test.QuickCheck

-- Semigroup Exercises
-- tested by this
semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

-- Monoid Exercises Added
--tested by
monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

-- One

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
    _ <> _ = Trivial

instance Monoid Trivial where
    mempty = Trivial
    _ `mappend` _ = Trivial

instance Arbitrary Trivial where
    arbitrary = return Trivial
    
type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool
   
trivCheck :: IO ()
trivCheck = do
    let sa = semigroupAssoc
        mli = monoidLeftIdentity
        mri = monoidRightIdentity
    quickCheck (sa :: TrivAssoc)
    quickCheck (mli :: Trivial -> Bool)
    quickCheck (mri :: Trivial -> Bool)
-- Two

newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
    (Identity a) <> (Identity b) = Identity (a <> b)

instance (Semigroup a, Monoid a) => Monoid (Identity a) where
    mempty = Identity mempty
    mappend = (<>)


instance Arbitrary a => Arbitrary (Identity a)
    where arbitrary = do
            a <- arbitrary
            return (Identity a)

type IdAssoc = Identity String -> Identity String -> Identity String -> Bool

idCheck :: IO ()
idCheck = do
    let sa = semigroupAssoc
        mli = monoidLeftIdentity
        mri = monoidRightIdentity
    quickCheck (sa :: IdAssoc)
    quickCheck (mli :: (Identity String) -> Bool)
    quickCheck (mri :: (Identity String) -> Bool)

-- Three

data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
    (Two a b) <> (Two c d) = Two (a <> c) (b <> d)

instance (Semigroup a, Monoid a, Semigroup b, Monoid b) => Monoid (Two a b) where
    mempty = Two mempty mempty
    mappend = (<>)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b)
    where arbitrary = do
            a <- arbitrary
            b <- arbitrary
            return (Two a b)

type TwoAssoc = Two String String -> Two String String -> Two String String -> Bool

twoCheck :: IO ()
twoCheck = do
    let sa = semigroupAssoc
        mli = monoidLeftIdentity
        mri = monoidRightIdentity
    quickCheck (sa :: TwoAssoc)
    quickCheck (mli :: (Two String String) -> Bool)
    quickCheck (mri :: (Two String String) -> Bool)

-- Four

data Three a b c = Three a b c deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
    (Three a b c) <> (Three a' b' c') = Three (a <> a') (b <> b') (c <> c')


instance (Semigroup a, Monoid a, Semigroup b, Monoid b, Semigroup c, Monoid c) => Monoid (Three a b c) where
    mempty = Three mempty mempty mempty
    mappend = (<>)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c)
    where arbitrary = do
            a <- arbitrary
            b <- arbitrary
            c <- arbitrary
            return (Three a b c)

type ThreeAssoc = Three String String String -> Three String String String -> Three String String String -> Bool

threeCheck :: IO ()
threeCheck = do
    let sa = semigroupAssoc
        mli = monoidLeftIdentity
        mri = monoidRightIdentity
    quickCheck (sa :: ThreeAssoc)
    quickCheck (mli :: (Three String String String) -> Bool)
    quickCheck (mri :: (Three String String String) -> Bool)
-- six

newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance (Semigroup BoolConj) where
    (BoolConj True) <> (BoolConj True) = BoolConj True
    _ <> _ = BoolConj False

instance Monoid BoolConj where
    mempty = BoolConj True
    mappend = (<>)

instance (Arbitrary BoolConj) where
    arbitrary = do
        a <- arbitrary
        return (BoolConj a)

type BoolConjAssoc = BoolConj -> BoolConj  -> BoolConj -> Bool

boolConjCheck :: IO ()
boolConjCheck = do
    let sa = semigroupAssoc
        mli = monoidLeftIdentity
        mri = monoidRightIdentity
    quickCheck (sa :: BoolConjAssoc)
    quickCheck (mli :: BoolConj -> Bool)
    quickCheck (mri :: BoolConj -> Bool)

-- seven

newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
    (BoolDisj True) <> _ = BoolDisj True
    _ <> (BoolDisj True) = BoolDisj True
    _ <> _ = BoolDisj False

instance Monoid BoolDisj where
    mempty = BoolDisj False
    mappend = (<>)

instance (Arbitrary BoolDisj) where
    arbitrary = do
        a <- arbitrary
        return (BoolDisj a)

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

boolDisjCheck :: IO ()
boolDisjCheck = do
    let sa = semigroupAssoc
        mli = monoidLeftIdentity
        mri = monoidRightIdentity
    quickCheck (sa :: BoolDisjAssoc)
    quickCheck (mli :: BoolDisj -> Bool)
    quickCheck (mri :: BoolDisj -> Bool)

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

semigroupCheck :: IO ()
semigroupCheck = do
    quickCheck (semigroupAssoc :: TrivAssoc) 
    quickCheck (semigroupAssoc :: IdAssoc)
    quickCheck (semigroupAssoc :: TwoAssoc)
    quickCheck (semigroupAssoc :: ThreeAssoc)
    quickCheck (semigroupAssoc :: BoolConjAssoc)
    quickCheck (semigroupAssoc :: BoolDisjAssoc)
    quickCheck (semigroupAssoc :: OrAssoc)

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

validate = do
    let failure :: String -> Validation String Int
        failure = Fail
        success :: Int -> Validation String Int
        success = Succ
    print $ success 1 <> failure "blah"
    print $ failure "woot" <> failure "blah"
    print $ success 1 <> success 2
    print $ failure "woot" <> success 2

main :: IO ()
main = do
    print "Checking Trivial:"
    trivCheck
    print "Checking Identity:"
    idCheck
    print "Checking Two:"
    twoCheck
    print "Checking Three:"
    threeCheck
    print "Checking BoolConj:"
    boolConjCheck
    print "Checking BoolDisj:"
    boolDisjCheck
    print "Checking Validation:"
    validate