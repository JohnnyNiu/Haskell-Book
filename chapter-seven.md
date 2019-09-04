# More Functional Patterns

* Haskell functions are first-class entities that
* can be values in expressions, lists, or tuples;
* can be passed as arguments to a function;
* can be returned from a function as a result;

## Arguments and Parameters

### Parameters

* Value with no parameters:
* `myNum :: Integer; myNum = 1`
* `myVal = myNum` therefore `myVal :: Integer`
* adding a parameter (parameterizing):
* `myVal f = myNum` therefore `myVal :: t -> Integer`
* t means f is polymorphic, could be any type
* `myVal f = f + myNum` gives f a type therefore `myVal :: Integer -> Integer`
* Difference between a value and a function is a function takes an argument
* `myVal f g = myNum` has type `myVal :: t -> t1 -> Integer`
* t and t1 could be different types but not necessarily

### Binding variables to values

* Applying a function binds parameters to values
* Types are bound to types and parameters are bound to a value
* Can also bind with let and where clauses

```
bindExp :: Integer -> String
bindExp x =
    let z = y + x in
        let y = 5 in
            "the integer was: "
            ++ show x ++ " and y was: "
            ++ show y ++ " and z was: "
            ++ show z
```

* This won't work because y is only declared inside its let clause so it's not in scope

```
bindExp :: Integer -> String
bindExp x =
    let x = 10; y = 5 in
      "the integer was: " ++ show x
      ++ " and y was: " ++ show y
```

* If we do `bindExp 9001` the 9001 won't matter as the innermost declaration takes precedence, called shadow
* Haskell has lexical scoping, the value of an entity depends where it is declared in the code

## Anonymous functions

* `triple :: Integer -> Integer; triple x = x * 3`
* in anonymous syntax:
* `(\x -> x * 3) :: Integer -> Integer`
* parentheses are needed otherwise the type will only be applied to 3
* `(\x -> x * 3) 5` applies the function on one line

### Exercises: Grab Bag

1. All are equivlent
2. d
3. Turn into anonymous lambda syntax
   1. `f = \n -> n + 1`
   2. `addFive = \x -> \y -> if (x > y then y else x) + 5`
   3. `mflip f x y = f x y`

### Utility of lambda syntax

* Usually use anonymous syntax when you pass a function to a higher order function and the function won't be used again in the program
* Named entities evaluate differently than anonymous so sometimes anonymous can be used for that reason

## Pattern Matching

* Way of matching values against patterns and binding variables to successful matches
* Been using it already without knowing it
* Patterns matched against values, or data constructors, not types
* Matching may fail and then proceed to next available pattern to try
* Proceeds from left to right and outside to inside

```
isItTwo :: Integer -> Bool
isItTwo 2 = True
isItTwo _ = False
```

### Handling all cases

* Underscore matches anything so you can't put it first or it will only return that
* Try to order from most specific to least specific
* Trust GHC's overlap warning
* If pattern match is incomplete then when data not handled is entered then bottom will be returned, a non-value that means the program couldn't handle the data
* Compiler will warn about incomplete patterns

### Pattern matching against data constructors

* Some data constructors take parameters, pattern matching exposes and makes use of the data

```
newtype Username =
    Username String

newtype AccountNumber =
    AccountNumber Integer

data User =
    UnregisteredUser
    | RegisteredUser Username AccountNumber
```

* User has two constructors for registered or unregistered
* Can use pattern matching to print a user based on its constructor

```
printUser :: User -> IO ()
printUser UnregisteredUser =
    putStrLn "UnregisteredUser"
printUser (RegisteredUser
        (Username name)
        (AccountNumber acctNum)) =
    putStrLn $ name ++ " " ++ show acctNum
```

#### Another example

```
data WherePenguinsLive =
    Galapagos
    | Antarctica
    | Australia
    | SouthAfrica
    | SouthAmerica
    deriving (Eq, Show)

data Penguin =
    Peng WherePenguinsLive
    deriving (Eq, Show)
```

* now can write a couple of functions using pattern matching

```
isSouthAfrica :: WherePenguinsLive -> Bool
isSouthAfrica SouthAfrica = True
isSouthAfrica Galapagos = False
isSouthAfrica Antarctica = False
isSouthAfrica Australia = False
isSouthAfrica SouthAmerica = False
```

* or shortened

```
isSouthAfrica' :: WherePenguinsLive -> Bool
isSouthAfrica' SouthAfrica = True
isSouthAfrica' _ = False
```

* Can also use pattern matching to unpack Penguin values and get the WherePenguinsLive value that is contained within

```
gimmeWhereTheyLive :: Penguin
                    -> WherePenguinsLive
gimmeWhereTheyLive (Peng whereitlives) =
    whereitlives
```

* more elaborate example exposing WherePenguinLives and matching the value in one match:

```
galapagosPenguin :: Penguin -> Bool
galapagosPenguin (Peng Galapagos) = True
galapagosPenguin _ = False

antarcticPenguin :: Penguin -> Bool
antarcticPenguin (Peng Antarctica) = True
antarcticPenguin _ = False

antarcticOrGalapagos :: Penguin -> Bool
antarcticOrGalapagos p =
    (galapagosPenguin p)
    || (antarcticPenguin p)
```

### Pattern matching tuples

* Instead of `f x y = ((snd x, snd y), (fst x, fst y))`
* Can do `f (a, b) (c, d) = ((b, d), (a, c))`
* Looks a lot more like the type declaration
* More examples:

```
addEmUp2 :: Num a => (a, a) -> a
addEmUp2 (x, y) = x + y

-- addEmUp2 could also be written like so
addEmUp2Alt :: Num a => (a, a) -> a
addEmUp2Alt tup = (fst tup) + (snd tup)

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

third3 :: (a, b, c) -> c
third3 (_, _, x) = x
```

### Exercises: Variety Pack

1. `k :: (a, b) -> a
   1. String, not same type
   2. k3
2. f (a, b, c) (d, e, f) = ((a, d), (c, f))

## Case expressions

```
funcZ x =
    case x + 1 == 1 of
      True -> "AWESOME"
      False -> "wut"

pal xs =
    case xs == reverse xs of
      True -> "yes"
      False -> "no"

greetIfCool :: String -> IO ()
greetIfCool coolness =
    case cool of
      True ->
        putStrLn "eyyyyy. What's shakin'?"
      False ->
        putStrLn "pshhhh."
    where cool =
          coolness == "downright frosty yo"
```

### Exercises: case practice

1. see file

## Higher-order functions

* HOFs are functions that accept functions as arguments
  
```
flip :: (a -> b -> c) -> b -> a -> c
flip f x y = f y x

myFlip :: (a -> b -> c) -> b -> a -> c
myFlip f = \ x y -> f y x
```

* Normally -> is right associative but using parentheses can change that as long as it makes sense for the function
* If you parenthesize to the left it represents a separate function
* Function has its own parameter and result and is the argument to the top level function

```
data Employee = Coder
            | Manager
            | Veep
            | CEO
            deriving (Eq, Ord, Show)

reportBoss :: Employee -> Employee -> IO ()
reportBoss e e' =
  putStrLn $ show e ++
        " is the boss of " ++
        show e'

employeeRank :: ( Employee
                -> Employee
                -> Ordering )
              -> Employee
              -> Employee
              -> IO ()
employeeRank f e e' =
  case f e e' of
    GT -> reportBoss e e'
    EQ -> putStrLn "Neither employee\
                    \ is the boss"
    LT -> (flip reportBoss) e e'
``` 

* accepts a function argument that has the type in the parentheses
* so can do `employeeRank compare Veep CEO`
* can add another compare style function:

```
codersRuleCEOsDrool :: Employee
                    -> Employee
                    -> Ordering
codersRuleCEOsDrool Coder Coder = EQ
codersRuleCEOsDrool Coder _ = GT
codersRuleCEOsDrool _ Coder = LT
codersRuleCEOsDrool e e' =
  compare e e'
```

* now we can do `employeeRank codersRuleCEOsDrool CEO Coder`


### Exercises: Artful Dodgy

1. see file for functions
   1. 1
   2. 11
   3. 22
   4. 21
   5. 12
   6. 11
   7. 21
   8. 22
   9. 31
   10. 23

## Guards

* `myAbs x = if x < 0 then (-x) else x` becomes

```
myAbs x
    | x < 0 = (-x)
    | otherwise = x
```

* | is called pipe
* each pipe begins a guard case
* each guard case has its own equals sign instead of putting an equals at the start
* otherwise always evaluates to True
* order from most restrictive case to least restrictive

```
bloodNa :: Integer -> String
bloodNa x
    | x < 135 = "too low"
    | x > 145 = "too high"
    | otherwise = "just right"
```

* Can use any expression in the guard block as long as it evaluates to Bool
* Can use where declarations inside of a guard block
* Don't have to use otherwise as the last case but it can be difficult to know if every possibility is covered so it's a good idea to include it

### Exercises: guard duty

1. Just returns the first case
2. Returns C because 90 meets the condition >=70
3. b
4. Any list
5. `pal :: Eq a => [a] -> Bool`
6. c
7. Any number
8. `numbers :: Num a => a -> a` wrong should be `(Ord a, Num a, Num b) => a -> b`

## Function composition

* Lets you combine functions so that the result of applying one gets passed to the next
* `(b -> c) -> (a -> b) -> (a -> c)`
* first brackets = function that takes take b returns c
* second = function that takes a returns b
* third = function that is returned, takes a returns c
* `(f . g) x = f (g x)` is composed function syntax
* (.) is composition operator and takes two function
* f is (b -> c), g is (a -> b)
* g is applied to x, the result of that is passed to f
* f is evaluated and returns result
* e.g. `negate . sum $ [1, 2, 3, 4, 5]`
* sum evaluates to 15, negate gives -15
* `$` is needed otherwise the function would evaluate before the functions composed because function application has higher precedence
* `take 5 . enumFrom $ 3`
* useful because it makes it easy to compose more than two functions
* e.g. `take 5 . filter odd . enumFrom $ 3`

## Pointfree style

* Refers to composing functions without specifying arguments
* Point refers to arguments, not to the composition operator
* Tidier on the page and lets the reader focus on the functions rather than the data
* e.g. `f . g = \x -> f (g x)`
* `f . g . h = \x -> f (g (h x))`
* `let f = negate . sum` 
* `f [1, 2, 3, 4, 5]` returns -15
* `let f = length . filter (== 'a')` `f "abracadabra"`  returns 5

```
add :: Int -> Int -> Int
add x y = x + y

addPF :: Int -> Int -> Int
addPF = (+)

addOne :: Int -> Int
addOne = \x -> x + 1

addOnePF :: Int -> Int
addOnePF = (+1)
```

## Demonstrating Composition

* putStr has type `putStr :: String -> IO ()`
* print is different: `print :: Show a => a -> IO ()`
* if `show :: Show a => a -> String` then we can implement print as
* `print a = (putStrLn . show) a` which becomes `print = putStrLn . show`
* The point of print is to compose putStrLn and show so the arguments aren't important

## Chapter Exercises

### Multiple Choice

1. d
2. d
3. a
4. b
5. a

### Let's write code

1. see file
   1. No, type is `Integral a => a -> (a, a)`
   2. see file
2. see file

## Definitions

1. Binding or bound is a common word used to indicate connection,
linkage, or association between two objects. In Haskell we’ll
use it to talk about what value a variable has, e.g., a parameter
variable is bound to an argument value, meaning the value is
passed into the parameter as input and each occurrence of
that named parameter will have the same value. Bindings as
a plurality will usually refer to a collection of variables and
functions which can be referenced by name.
```
blah :: Int
blah = 10
```
Here the variable blah is bound to the value 10.
2. An anonymous function is a function which is not bound to an
identifier and is instead passed as an argument to another function
and/or used to construct another function. See the following
examples.
```
\x -> x
-- anonymous version of id
id x = x
-- not anonymous, it's bound to 'id'
```
3. Currying is the process of transforming a function that takes
multiple arguments into a series of functions which each take
one argument and return one result. This is accomplished
through the nesting. In Haskell, all functions are curried by
default. You don’t need to do anything special yourself.
```
-- curry and uncurry already
-- exist in Prelude
curry' :: ((a, b) -> c) -> a -> b -> c
curry' f a b = f (a, b)
uncurry' :: (a -> b -> c) -> ((a, b) -> c)
uncurry' f (a, b) = f a b
-- uncurried function,
-- takes a tuple of its arguments
add :: (Int, Int) -> Int
add (x, y) = x + y
add' :: Int -> Int -> Int
add' = curry' add
A function that appears to take two arguments is two functions
that each take one argument and return one result. What makes
this work is that a function can return another function.
f a b = a + b
-- is equivalent to
f = \a -> (\b -> a + b)
```
1. Pattern matching is a syntactic way of deconstructing product and
sum types to get at their inhabitants. With respect to products,
pattern matching gives you the means for destructuring and
exposing the contents of products, binding one or more values
contained therein to names. With sums, pattern matching lets
you discriminate which inhabitant of a sum you mean to handle
in that match. It’s best to explain pattern matching in terms
of how datatypes work, so we’re going to use terminology that
you may not fully understand yet. We’ll cover this more deeply
soon.
```
-- nullary data constructor,
-- not a sum or product.
-- Just a single value.
data Blah = Blah
Pattern matching on Blah can only do one thing.
blahFunc :: Blah -> Bool
blahFunc Blah = True
data Identity a =
Identity a
deriving (Eq, Show)
Identity is a unary data constructor. Still not a product, only
contains one value.
-- when you pattern match on Identity
-- you can unpack and expose the 'a'
unpackIdentity :: Identity a -> a
unpackIdentity (Identity x) = x
-- But you can choose to ignore
-- the contents of Identity
ignoreIdentity :: Identity a -> Bool
ignoreIdentity (Identity _) = True
-- or ignore it completely since
-- matching on a non-sum data constructor
-- changes nothing.
ignoreIdentity' :: Identity a -> Bool
ignoreIdentity' _ = True
data Product a b =
Product a b
deriving (Eq, Show)
```
Now we can choose to use none, one, or both of the values in
the product of 𝑎 and 𝑏:
```
productUnpackOnlyA :: Product a b -> a
productUnpackOnlyA (Product x _) = x
productUnpackOnlyB :: Product a b -> b
productUnpackOnlyB (Product _ y) = y
Or we can bind them both to a different name:
productUnpack :: Product a b -> (a, b)
productUnpack (Product x y) = (x, y)
```
What happens if you try to bind the values in the product to
the same name?
```
data SumOfThree a b c =
FirstPossible a
| SecondPossible b
| ThirdPossible c
deriving (Eq, Show)
```
Now we can discriminate by the inhabitants of the sum and
choose to do different things based on which constructor in the
sum they were.
```
sumToInt :: SumOfThree a b c -> Integer
sumToInt (FirstPossible _) = 0
sumToInt (SecondPossible _) = 1
sumToInt (ThirdPossible _) = 2
-- We can selectively ignore
-- inhabitants of the sum
sumToInt :: SumOfThree a b c -> Integer
sumToInt (FirstPossible _) = 0
sumToInt _ = 1
-- We still need to handle
-- every possible value
```
Pattern matching is about your data.
1. Bottom is a non-value used to denote that the program cannot
return a value or result. The most elemental manifestation of
this is a program that loops infinitely. Other forms can involve
things like writing a function that doesn’t handle all of its inputs
and fails on a pattern match. The following are examples of
bottom:
```
-- If you apply this to any values,
-- it'll recurse indefinitely.
f x = f x
-- It'll a'splode if you pass a False value
dontDoThis :: Bool -> Int
dontDoThis True = 1
-- morally equivalent to
definitelyDontDoThis :: Bool -> Int
definitelyDontDoThis True = 1
definitelyDontDoThis False = error "oops"
-- don't use error.
-- We'll show you a better way soon.
```
Bottom can be useful as a canary for signaling when code paths
are being evaluated. We usually do this to determine how lazy
a program is or isn’t. You’ll see a lot of this in our chapter on
non-strictness later on.
1. Higher-order functions are functions which themselves take functions
as arguments or return functions as results. Due to currying,
technically any function that appears to take more than
one argument is higher order in Haskell.