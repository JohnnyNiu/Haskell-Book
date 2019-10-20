# Testing

## Quick tour of testing

* possible to write well-typed code that doesn't perform as expected
* tests allow to state an expectation and verify the result of an operation meets that expectation
* two broad categories of testing: unit testing and property testing
* unit testing tests smallest atomic units of software independently
* checks that weach function is performing task it is meant for by asserting that when the code runs with a specified input, the result is equal to the expected result
* spec testing does this too, but checks that the expected result is equal to the actual result
* spec testing can be preferred because it's written in terms of assertions that are in human-readable language
* Haskell offers hspec library for spec testing which will be used in this chapter

### Property testing

* this was pioneered in haskell because of the type system and straightforward logic of the language
* since adopted by other languages
* tests the formal properties of programs without requiring formal proofs
* by allowing you to express a truth-valued, universally quantified function that will apply to all cases. usually uses equality, and checks against randomly generated inputs
* inputs are generated randomly using the QuickCheck library
* relied on type system to know what data to generate
* by default tests 100 inputs, giving 100 results, if it fails one of these then the program doesn't have that property
* if it passes, doesn't guarantee it will never fail because of weird edge cases
* QuickCheck is designed to be as thorugh as possible and will check common edge cases
* prop testing not appropriate for all programs but useful for checking that program satisfies minimum requirements for laws, like laws of monads or basic associativity

## Conventional Testing

* hspec won't be explained deeply, chapter is more designed to learn how to write tests
* see addition project for example of testing
* be careful with importing hspec, must go before declarations and must be included in build-depends
* can use `:browse` to see what's contained in Test.Hspec, this is often more useful with libraries you are familiar with since it is dense


### Our first hspec test

* see Addition.hs for code, commented out block
* `shouldBe :: (Eq a, Show a) => a -> a -> Expectation`
* comapre to `(==) :: Eq a => a -> a -> Bool`
* essentially verifies that arguments are equal
* Show allows to print result of test
* now will test recursive division function from earlier chapter
* see new main function

### Intermission: Short exercise

* see Addition.hs

## Enter QuickCheck

* hspec can only prove something about particular values
* need assurances that are stronger
* need to add QuickCheck to build-depends in cabal file
* see file for example

### Arbitrary instances

* QuickCheck relies on a typeclass called Arbitrary and a newtype called Gen for generating random data
* `arbitrary :: Arbitrary a => Gen a`
* is a way of setting a default generator for a type
* have to specify the type to dispatch the right typeclass instance
* can use sample and sample' to see some random data
* `sample :: Show a => Gen a -> IO ()` prints each value on a new line
* `sample' :: Gen a -> IO [a]` returns a list of values
* IO is because it uses a global resource of random values to generate the data
* common way of generating pseudorandom data is to have a function that takes a seed value and returns a value and another seed value
* can bind those two actions together to pass a new seed value each time and get seemingly random data
* this is a bit different, in this case it uses IO so that it can return a different result each time by pulling from a global resource of random valeus
* has to use IO because pure functions don't allow this kind of behaviour
* use the Arbnitrary typeclass to provide a generator for sample
* convenient way of pulling a generator for Gen almost out of thin air
* Gen is a newtype that exists for wrapping a function to generate pseudorandom values
* takes an argument and gives a pseudorandom value of that type

```Haskell
sample (arbitrary :: Gen Int)
0
-2
-1
4
-3
4
2
4
-3
2
-4

sample (arbitrary :: Gen Double)
0.0
0.13712502861905426
2.9801894108743605
-8.960645064542609
4.494161946149201
7.903662448338119
-5.221729489254451
31.64874305324701
77.43118278366954
-539.7148886375935
26.87468214215407
```

* can specify our own data for generating Gen values
* function that always returns a 1 of type Int:

```Haskell
trivialInt :: Gen Int
trivialInt = return 1
```

* uses return, which was used with IO in last chapter
* isn't limited to IO, can be used with any Monad
* putting 1 into Gen monad creates a generator that always returns 1

```Haskell
sample' trivialInt
[1,1,1,1,1,1,1,1,1,1,1]
```

* another way of generating values:

```Haskell
oneThroughThree :: Gen Int
oneThroughThree = elements [1, 2, 3]

sample' oneThroughThree
[2,3,3,2,2,1,2,1,1,3,3]
```

* generates a random values only from the specified set
* can play with the odds by having values appear different numbers of times

```Haskell
oneThroughThree :: Gen Int
oneThroughThree = elements [1, 2, 2, 2, 2, 3]
```

* see Addition.hs for examples involving choose
* also see file for tuple examples
* can use tuple generators by specifying type 
* `sample (genTuple :: Gen (Int, Float))`
* can ask for anything that has an instance of the Arbitrary typeclass
* e.g. `sample (genTuple :: Gen ([()], Char))`
* can also generate arbitrary Maybe and Either values
* see file

### Using QuickCheck without Hspec

* can also use QuickCheck without hspec

```Haskell
prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 1 > x

runQc :: IO ()
runQc = quickCheck prop_additionGreater
```

* now call runQc instead of main, which will directly run QuickCheck
* specifies the number of tests it ran, or how many it ran before it failed
* lets you know what value it failed on

## Morse Code

* see morse project
* see cabal for setup
* Morse.hs for code

### Turning words into code

* import of Data.Map is qualified
* map as a data structure will be covered more later in book
* can be understood as a balanced binary tree
* each node is a pairing of key and value
* key is an index for the value
* key must be orderable
* maps can be more efficient than lists due to not having to search linearly through data
* can find the key a lot quicker since the tree is cut in half at each comparison
* want to make a list of pairs, each pair includes the english letter and its morse equivalent
* see file for transliteration table

### The Main event

* see Main.hs
* not all code will be explained in detail because focus is on testing
* can run by typing `echo "hi" | stack exec morse to`
* or `echo ".... .." | stack exec morse from`

### Time to test

* now need to write a test suite
* needs own directory, test
* has own Main module inside of tests.hs
* see file for code

### Testing the morse code

* can now run tests to make sure conversions are working
* can open a REPL from main project directory using:
* `stack ghci morse:tests`
* then run main to run tests
* generates 100 random morse code conversions and makes sure they are equal once converted there and back
* gives a pretty strong assurance program is working correctly

## Arbitrary instances

* important part of QuickCheck is learning to write instances of Arbitrary for your datatypes
* can be a bit confusing but is necessary for the convenience of integrating with QuickCheck

### Babby's First Arbitrary

* starts with simplest possible Arbitrary instance
* see arbitrary project

### Identity Crisis

* this is different, will produce random values even if the Identity structure itself doesn't and cannot vary
* see file
* can change the concrete type of identity's type argument to make different sample values

### Arbitrary Products

* arbitrary instances for product types are slightly more interesting
* but just an extension of the Identity example
* see file

### Greater than the sum of its parts

* sum types are more interesting still
* must include `import Test.QuickCheck.Gen (oneof)`
* sum types represent disjunction, so need to represent the exclusive possibilities in the Gen
* one way is to pull out as many arbitrary values as required for the cases of the sum type
* see file
* oneof creates a Gen a from a list of Gen a by giving each value equal probability
* can choose a different weighting of probabilities 
* snippet of Maybe Arbitrary instance from QuickCheck library:

```Haskell
instance Arbitrary a =>
        Arbitrary (Maybe a) where
    arbitrary =
        frequency [(1, return Nothing),
                   (3, liftM Just arbitrary)]
```

* makes Just value three times more likely than Nothing value
* Just is more likely to be interesting and useful but still want to test Nothing
* see file for relevant example
* Arbitrary instance for a datatype doesn't have to be the only way to generate random values for a datatype
* can offer alternate Gens with useful behaviour

### CoArbitrary

* counterpart to Arbitrary enables generation of functions fitting particular type
* can provide functions with value of type a as an argument to vary a Gen

```Haskell
arbitrary :: Arbitrary a => Gen a

coarbitrary :: CoArbitrary a => a -> Gen b -> Gen b
```

* first argument is used to return a modification of second argument
* as long as datatype has a Generic instance defined, can get these instances for free
* see file
* essentially lets you randomly generate a function
* not immediately important, but if you need to randomly generate something with a (->) type, becomes useful quickly

## Chapter Exercises

1. see WordNumberTest.hs
2. see UsingQuickCheck.hs

### Failure

Fails because sqrt returns a floating point number which may not be exactly the same as the original Integer due to the imprecision of floating point numbers in general

### Idempotence

see Idempotence.hs

### Make a Gen random generator for the datatype

see Generators.hs

### Hangman testing

see tests in hangman project from last chapter

## Chapter Definitions

1. Unit testing is a method in which you test the smallest parts
of an application possible. These units are individually and
independently scrutinized for desired behaviors. Unit testing is
better automated but it can also be done manually via a human
entering inputs and verifying outputs.
2. Property testing is a testing method where a subset of a large
input space is validated, usually against a property or law some
code should abide by. In Haskell, this is usually done with
QuickCheck which facilitates the random generation of input and
definition of properties to be verified. Common properties
that are checked using property testing are things like identity,
associativity, isomorphism, and idempotence.
3. When we say an operation or function is idempotent or satisfies
idempotence, we mean that applying it multiple times doesn’t
produce a different result from the first time. One example is
multiplying by one or zero. You always get the same result as
the first time you multipled by one or zero.