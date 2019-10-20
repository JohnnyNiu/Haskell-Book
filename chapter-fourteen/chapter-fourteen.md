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