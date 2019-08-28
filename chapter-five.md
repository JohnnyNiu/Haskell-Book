# Types

* Haskell based on a form of typed lambda calculus called System F
* Types impose constraints that enforce correctness
* Type system checks that parts of program fit together in a logically consistent, provably correct way
* Typechecking occurs at compile time

## Function Type

* Functions are values
* -> is the type constructor for functions
* arrow is infix operator has two parameters and associates to the right
* first parameter is bound variable, second is the result
  
### Typeclass constrained functions

* Function applies least specific typeclass possible, for arithmetic operations the Num type
* This means the variable is a typeclass-constrained polymorphic type variable
* Could be any of the types that have an instance of that typeclass
* `(Ord a, Num a)` represents conjunction of constraints on a variable
  
### Exercises: Type Matching

1. c
2. d
3. b
4. a
5. e

## Currying

* Haskell doesn't have inbuilt support for multiple parameters but has shorthand for applying one function after the other
* `(+) :: Num a => a -> a -> a`
* each small arrow represents application of a function
* since it associates to the right `f :: a -> a ->` can be read as `f :: a -> (a  -> a)`
* functions however evaluate to the left, so the outermost evaluates first

### Partial application

```
subtractStuff :: Integer
-> Integer
-> Integer
subtractStuff x y = x - y - 10
subtractOne = subtractStuff 1
```

* subtractOne is partial application of subtractStuff, applying one argument, then second argument is applied to subtractOne

### Manual currying and uncurrying

* These are all the same function with different levels of currying, \ represents anonymous functions where \ takes the place of the lambda:
  
```
nonsense :: Bool -> Integer
nonsense True = 805
nonsense False = 31337

curriedFunction :: Integer
                -> Bool
                -> Integer
curriedFunction i b =
    i + (nonsense b)

uncurriedFunction :: (Integer, Bool)
                    -> Integer
uncurriedFunction (i, b) =
    i + (nonsense b)

anonymous :: Integer -> Bool -> Integer
anonymous = \i b -> i + (nonsense b)

anonNested :: Integer
            -> Bool
            -> Integer
anonNested =
    \i -> \b -> i + (nonsense b)
```

* This shows that functions that seem to accept multiple arguments are actually higher order functions, functions that accept functions as their arguments

### Sectioning

* Partial application of infix operators, can choose the order of operations
* e.g. `let y = (2^)` and `let z = (^2)` are different functions
* with commutative functions, order doesn't matter
* doesn't just apply to arithmetic, and can be used by making non-infix infix through backticks e.g. `let c = (`elem` [1..10])`

### Exercises: Type arguments

1. a
2. d
3. b wrong, Num b => b
4. c
5. a
6. d wrong, Eq b => b -> Char, can't leave out the left hand side because it puts constraints on the typeclass of the variable
7. d
8. a
9. c

## Polymorphism

* Three types of type signatures: concrete, parametric polymorphism, constrained polymorphism
* Constrained (or ad-hoc) polymorphism implemented with typeclasses
* Parametric polymorphism means parameters that are fully polymorphic
* Not constrained by typeclasses so concrete type could be anything
* `id a -> a` a can take any concrete type and returns that type
* `Num a => a -> a` is constrained and can take a numeric type
* More constrained a variable is, the more that can be done with it because we know more about it
* Function is polymorphic when its type signature can represent variables of more than one type

### Exercises: Parametricity

1. Not possible
2. `f :: a -> a -> a`  `f x y = x` `f x y = y`
3. `g :: a -> b -> b`  `g x y = y`

### Polymorphic constants

* Haskell will keep something as most generic typeclass until it needs to evaluate
* e.g. (-10) + 6.3, -10 is Num and 6.3 is Fractional so Num becomes Fractional so that expression can evaluate
* Can use fromIntegral to turn an integral value back into Num typeclass

## Type inference

* Haskell's type inference built on the Damas-Hindley-Milner type system
* Infers the most polymorphic type that is still correct
* Compiler works from types it knows to work out ones it doesn't
* Good practice to be explicit about types once you know what they are

### Exercises: apply yourself

1. `myConcat :: [Char] -> [Char]`
2. `myMult :: Fractional a => a -> a`
3. `myTake :: Int -> [Char]`
4. `myCom :: Int -> Bool`
5. `myAlph :: Char -> Bool`

## Asserting Types

* Can force a certain type with type declaration e.g. `triple :: Integer -> Integer`
* Can also do `triple x = x * 3 :: Integer`
* This is also possible though uncommon:
  
```
triple x = tripleItYo x
    where tripleItYo :: Integer -> Integer
            tripleItYo y = y * 3
```

* If you try and force a type but use operations that aren't defined for that type, it will throw an error

## Chapter Exercises

### Multiple Choice

1. c
2. a
3. b
4. c

### Determine the Type

1. 
   1. 54, Num a => a
   2. (0, "doge"), Num a => (a, [Char])
   3. (0 :: Integer, "doge"), (Integer, [Char])
   4. False, Bool
   5. 5, Int
   6. False, Bool
2. Num a => a
3. Num a => a -> a
4. Fractional a => a
5. [Char]

### Does it compile?

1. See file

### Type variable or specific type constructor

2. zed = fully polymorphic, Zed = concrete, Blah = concrete
3. a = fully, b = constrained, C = concrete
4. f = fully, g = fully, C = concrete

### Write a type signature

1. `functionH :: [a] -> a`
2. `functionC :: Ord a => a -> a -> Bool`
3. `functionS :: (a, b) -> b`

### Given a type, write a function

1. `i x = x`
2. `c x y = x`
3. `c'' x y = x`, alpha equivalence means same as 2
4. `c' x y = y`
5. `r a = take 3 a` `r a = tail a`
6. `co bToC AtoB a = bToC (AtoB a)`
7. `a cToA aToC a = cToA (aToC a)`
   1. Wrong, should be `a xToZ x = x`
8. `a' xToY x = xToY x`

### Fix it

1. See file

### Type-Kwon-Do

1. `h x = g $ f x`
2. `e x = w $ q x`
3. `xform (x, y) = (xz a, yz y)
4. `munge xy ywz x = fst (ywz $ xy x)`

## Definitions

1. Polymorphism refers to type variables which may refer to more
than one concrete type. In Haskell, this will usually manifest as
parametric or ad-hoc polymorphism. By having a larger set of
types, we intersect the commonalities of them all to produce
a smaller set of correct terms. This makes it less likely we’ll
write an incorrect program and lets us reuse the code with other
types.

2. Type inference is a faculty some programming languages, most
notably Haskell and ML, have to infer principal types from terms
without needing explicit type annotations. There are, in some
cases, terms in Haskell which can be well-typed but which have
no principal type. In those cases, an explicit type annotation
must be added.
With respect to Haskell, the principal type is the most generic
type which still typechecks. More generally, Principal type is a
property of the type system you’re interacting with. Principal
typing holds for that type system if a type can be found for a
term in an environment for which all other types for that term
are instances of the principal type. Here are some examples:

```
-- Given the inferred types
a
Num a => a
Int
-- The principal type here is the
-- parametrically polymorphic 'a'.
-- Given these types
(Ord a, Num a) => a
Integer
-- The principal type is
-- (Ord a, Num a) => a
```

3. Type variable is a way to refer to an unspecified type or set of
types in Haskell type signatures. Type variables ordinarily will
be equal to themselves throughout a type signature. Let us
consider some examples.

```
id :: a -> a
-- One type variable 'a' that occurs twice,
-- once as an argument, once as a result.
-- Parametrically polymorphic, could be
-- strictly anything
(+) :: Num a => a -> a -> a
-- One type variable 'a', constrained
-- to needing an instance of Num. Two
-- arguments, one result.
-- All the same type.
```

4. A typeclass is a means of expressing faculties or interfaces that
multiple datatypes may have in common. This enables us to
write code exclusively in terms of those commonalities without
repeating yourself for each instance. Just as one may sum values
of type Int, Integer, Float, Double, and Rational, we can avoid
having different (+), (*), (-), negate, etc. functions for each by
unifying them into a single typeclass. Importantly, these can
then be used with all types that have a Num instance. Thus, a
typeclass provides us a means to write code in terms of those
operators and have our functions be compatible with all types
that have instances of that typeclass, whether they already exist
or are yet to be invented (by you, perhaps).
5. Parametricity is the property that holds in the presence of parametric
polymorphism. Parametricity states that the behavior
of a function will be uniform across all concrete applications of
the function. Parametricity4 tells us that the function:
`id :: a -> a`
Can be understood to have the same exact behavior for every
type in Haskell without us needing to see how it was written. It
is the same property that tells us:
`const :: a -> b -> a`
const must return the first value — parametricity and the definition
of the type requires it!
`f :: a -> a -> a`
Here, 𝑓 can only return the first or second value, nothing else,
and it will always return one or the other consistently without
changing. If the function 𝑓 made use of (+) or (*), its type would
necessarily be constrained by the typeclass Num and thus be
an example of ad-hoc, rather than parametric, polymorphism.
`blahFunc :: b -> String`
blahFunc totally ignores its argument and is effectively a constant
value of type String which requires a throw-away argument for
no reason.
`convList :: a -> [a]`
Unless the result is [], the resulting list has values that are all
the same value. The list will always be the same length.
6. Ad-hoc polymorphism (sometimes called “constrained polymorphism”)
is polymorphism that applies one or more typeclass
constraints to what would’ve otherwise been a parametrically
polymorphic type variable. Here, rather than representing a
uniformity of behavior across all concrete applications, the purpose
of ad-hoc polymorphism is to allow the functions to have
different behavior for each instance. This ad-hoc-ness is constrained
by the types in the typeclass that defines the methods
and Haskell’s requirement that typeclass instances be unique
for a given type. For any given combination of typeclass and
a type, such as Ord and Bool, there must only exist one unique
instance in scope. This makes it considerably easier to reason
about typeclasses. See the example for a disambiguation.

```
(+) :: Num a => a -> a -> a
-- the above function is leveraging
-- ad-hoc polymorphism via the
-- Num typeclass
c' :: a -> a -> a
-- This function is not,
-- it's parametrically polymorphic in 'a'.
```

7. A module is the unit of organization that the Haskell programming
language uses to collect together declarations of values,
functions, data types, typeclasses, and typeclass instances. Any
time you use “import” in Haskell, you are importing declarations
from a module. Let us look at an example from the chapter
exercises:

```
{-# LANGUAGE NoMonomorphismRestriction #-}
module DetermineTheType where
-- ^ name of our module
Here we made our Haskell source file have a module and we
named it DetermineTheType. We included a directive to the compiler
to disable the monomorphism restriction before we declared
the module. Also consider the following example using
import:
import Data.Aeson (encode)
-- ^ the module Data.Aeson
import Database.Persist
-- ^ the module Database.Persist
```

In the above example, we are importing the function encode
declared in the module Data.Aeson along with any typeclass instances.
With the module Database.Persist we are importing
everything it makes available.