{-# LANGUAGE DeriveGeneric #-}
module Main where

import Test.QuickCheck
import Test.QuickCheck.Gen (oneof)
import GHC.Generics

-- SECTION 1
-- simplest possible generator

data Trivial = Trivial deriving (Eq, Show)

trivialGen :: Gen Trivial
trivialGen = return Trivial

instance Arbitrary Trivial where
  arbitrary = trivialGen

main :: IO () 
main = do
  sample trivialGen

-- SECTION 2
-- using Identity instead

data Identity a = Identity a deriving (Eq,Show)

identityGen :: Arbitrary a => Gen (Identity a)
identityGen = do
  a <- arbitrary
  return (Identity a)

-- using Gen monad to pluck a single value of type a out of the air
-- then embeds it in identity and returns it as part of Gen monad

-- make it the default generator for the Identity type by making it the
-- arbitrary value in the Arbitrary instance

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = identityGen

identityGenInt :: Gen (Identity Int)
identityGenInt = identityGen

identityGenChar :: Gen (Identity Char)
identityGenChar = identityGen

-- SECTION 3
-- product types

data Pair a b = Pair a b deriving (Eq, Show)

pairGen :: (Arbitrary a, Arbitrary b) => Gen (Pair a b)
pairGen = do
  a <- arbitrary
  b <- arbitrary
  return (Pair a b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
  arbitrary = pairGen

pairGenIntString :: Gen (Pair Int String)
pairGenIntString = pairGen
-- randomly generated string values can get a little weird

-- SECTION 4
-- SUM TYPES

data Sum a b = First a | Second b deriving (Eq, Show)

-- equal odds for each
sumGenEqual :: (Arbitrary a, Arbitrary b) => Gen (Sum a b)
sumGenEqual = do
  a <- arbitrary
  b <- arbitrary
  oneof [return $ First a, return $ Second b]

-- oneof creates a Gen a from a list of Gen a by giving each value equal probability

sumGenCharInt :: Gen (Sum Char Int)
sumGenCharInt = sumGenEqual

-- making First 10 times more likely:

sumGenFirstPls :: (Arbitrary a, Arbitrary b) => Gen (Sum a b)
sumGenFirstPls = do
  a <- arbitrary
  b <- arbitrary
  frequency [(10, return $ First a), (1, return $ Second b)]

sumGenCharIntFirst :: Gen (Sum Char Int)
sumGenCharIntFirst = sumGenFirstPls

-- SECTION 5
-- CoArbitrary

data Bool' = True' | False' deriving (Generic)

instance CoArbitrary Bool'

trueGen :: Gen Int
trueGen = coarbitrary True' arbitrary

falseGen :: Gen Int
falseGen = coarbitrary False' arbitrary

