module Exercises where

import Control.Monad
import Data.Monoid
import Test.QuickCheck

-- Optional 

data Optional a =
    Nada
    | Only a
    deriving (Eq, Show)

-- instance Monoid a => Monoid (Optional a) where
--     mempty = Nada
--     (Only a) `mappend` (Only b) = Only (a `mappend` b)
--     (Only a) `mappend` Nada = Only a
--     Nada `mappend` (Only b) = Only b
--     Nada `mappend` Nada = Nada

-- Madness

type Verb = String
type Adjective = String
type Adverb = String
type Noun = String
type Exclamation = String

madlibbin' :: Exclamation
    -> Adverb
    -> Noun
    -> Adjective
    -> String
madlibbin' e adv noun adj =
    e <> "! he said " <>
    adv <> " as he jumped into his car " <>
    noun <> " and drove off with his " <>
    adj <> " wife."

madlibbinBetter'  :: Exclamation
    -> Adverb
    -> Noun
    -> Adjective
    -> String
madlibbinBetter' e adv noun adj = mconcat [e, "! he said ", adv, " as he jumped into his car ", noun, " and drove off with his ", adj, " wife."]

-- Testing QuickCheck

data Bull = Fools | Twoo deriving (Eq, Show)
    
instance Arbitrary Bull where
    arbitrary = 
        frequency [ (1, return Fools)
        , (1, return Twoo) ]
 
-- this instance doesn't compile for some reason
-- instance Monoid Bull where
--     mempty = Fools
--     mappend _ _ = Fools
    
-- type BullMappend = Bull -> Bull -> Bull -> Bool

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

-- main :: IO ()
-- main = do
--     let ma = monoidAssoc
--         mli = monoidLeftIdentity
--         mlr = monoidRightIdentity
--     quickCheck (ma :: BullMappend)
--     quickCheck (mli :: Bull -> Bool)
--     quickCheck (mlr :: Bull -> Bool)

-- Maybe Another Monoid

newtype First' a =
    First' { getFirst' :: Optional a }
    deriving (Eq, Show)
    
instance Monoid (First' a) where
    mempty = First' {getFirst' = Nada}
    mappend = firstMappend

instance Arbitrary a => Arbitrary (First' a) where
    arbitrary = do
        a <- arbitrary
        frequency [ 
            (1, return $ First' Nada),
            (2, return $ First' (Only a))]

firstMappend :: First' a -> First' a -> First' a
firstMappend (First' (Only a)) _ = First' {getFirst' = Only a}
firstMappend _ (First' (Only a)) = First' {getFirst' = Only a}
firstMappend _ _ = First' {getFirst' = Nada}

type FirstMappend = First' String -> First' String -> First' String -> Bool

type FstId = First' String -> Bool
    
main :: IO ()
main = do
    quickCheck (monoidAssoc :: FirstMappend)
    quickCheck (monoidLeftIdentity :: FstId)
    quickCheck (monoidRightIdentity :: FstId)