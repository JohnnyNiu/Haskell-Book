# Typeclasses

## What are Typeclasses

* “The goal is to define a
datatype by cases, where one can add new cases to the datatype and
new functions over the datatype, without recompiling existing code,
and while retaining static type safety (e.g., no casts).”
* Comparable in usage to interfaces in object oriented, enabling polymorphism
* Defines a common set of features across different types

## Bool

* Even something simple like Bool has instances of 6 different typeclasses
* Instances means type defines how values and functions from typeclass works for that type
* Typeclasses can have a hierarchy, Fractional has to be Num, Ord has to be Eq since checking equality is necessary to order things

## Eq

* Eq defined as:
```
class Eq a where
(==) :: a -> a -> Bool
(/=) :: a -> a -> Bool
```
* Applying == to arguments binds a to a particular datatype
* e.g. `Eq Integer => Integer -> Integer -> Bool`
* Certain typeclasses like Eq can be derived which means they can be used without writing extra code manually

## Writing typeclass instances

* Eq can be derived instead of writing an instance but makes good example
* Documentation states: Minimal complete definition: either == or /=.
* Meaning that at least one has to be defined, the other can be the negation of the first
* `data Trivial = Trivial` simplest possible datatype
* `Trivial == Trivial` returns error because no definition for Eq. Some languages provide this by default but it's not completely safe so Haskell does not. Same with Show (print)

```
data Trivial =
    Trivial'

instance Eq Trivial where
    Trivial' == Trivial' = True
```

* is all that's needed for Eq
* single quote is to indicate that Trivial' is a value, Trivial is the datatype. This is not normally done in Haskell
* Now `Trivial' == Trivial'` returns true
  
### Less trivial example

```
data DayOfWeek =
Mon | Tue | Weds | Thu | Fri | Sat | Sun

data Date =
Date DayOfWeek Int
```

* Defined types for the day of the week and the numerical day

Eq for DayOfWeek:

```
instance Eq DayOfWeek where
    (==) Mon Mon = True
    (==) Tue Tue = True
    (==) Weds Weds = True
    (==) Thu Thu = True
    (==) Fri Fri = True
    (==) Sat Sat = True
    (==) Sun Sun = True
    (==) _ _ = False
```

For Date:

```
instance Eq Date where
    (==) (Date weekday dayOfMonth)
        (Date weekday' dayOfMonth') =
        weekday == weekday'
    && dayOfMonth == dayOfMonth'
```

* Uses Eq definitions of DayOfWeek and Int to define
* `Date Thu 10 == Date Thu 10` now returns true

### Partial functions

* definitions for Eq must be exhaustive, if some case isn't considered then it'll return an error
* doesn't just apply to typeclass
* consider `f :: Int -> Bool; f 2 = True` - doesn't consider every other Int
* can use _ to have fallback or use a datatype that's not as big as Int

### Another case

```
data Identity a =
    Identity a

instance Eq (Identity a) where
    (==) (Identity v) (Identity v') = v == v'
```

* issue is that a isn't necessarily Eq so v == v' might not make sense

```
instance Eq a => Eq (Identity a) where
    (==) (Identity v) (Identity v') = v == v'
```

* ensures that a has an instance of Eq

### Eq exercises

1. see file

## Num

Defined as:

```
class Num a where
    (+) :: a -> a -> a
    (*) :: a -> a -> a
    (-) :: a -> a -> a
    negate :: a -> a
    abs :: a -> a
    signum :: a -> a
    fromInteger :: Integer -> a
```

### Integral

Defined as:

```
class (Real a, Enum a) => Integral a where
    quot :: a -> a -> a
    rem :: a -> a -> a
    div :: a -> a -> a
    mod :: a -> a -> a
    quotRem :: a -> a -> (a, a)
    divMod :: a -> a -> (a, a)
    toInteger :: a -> Integer
```

* Must have instances for Real and Enum
* Real has an instance of Num
* This inheritance is only additive, no ambiguity problems possible

### Fractional

```
class (Num a) => Fractional a where
    (/) :: a -> a -> a
    recip :: a -> a
    fromRational :: Rational -> a
```

* Fractional has access to all properties of Num but Num doesn't have acces to things such a (/) so typeclass must be more specific i.e. Fractional or inheriting from Fractional

## Type-defaulting typeclasses

* Usually when evaluating the type signature will be used to get a concrete type or else type inference will be used
* Sometimes this is not possible and so a type must be supplied that has instances of all required typeclasses
* e.g. 1 / 2 defaults to Double and returns 0.5
* all default numerical typeclasses default to Integer or Double
* cannot make a type more general after going to a concrete type e.g. Integer -> Num without using fromInteger

## Ord

```
class Eq a => Ord a where
    compare :: a -> a -> Ordering
    (<) :: a -> a -> Bool
    (>=) :: a -> a -> Bool
    (>) :: a -> a -> Bool
    (<=) :: a -> a -> Bool
    max :: a -> a -> a
    min :: a -> a -> a
```

* `compare a a'` return LT, GT or EQ based on where the first argument is less than, greater than or equal to second argument
* `max a a'` and `min a a'` return the argument that is greater
  
### Ord instances

```
data DayOfWeek =
    Mon | Tue | Weds | Thu | Fri | Sat | Sun
    deriving (Ord, Show)
```

or can write our own Ord to make Friday more important:

```
data DayOfWeek =
    Mon | Tue | Weds | Thu | Fri | Sat | Sun
    deriving (Eq, Show)

instance Ord DayOfWeek where
    compare Fri Fri = EQ
    compare Fri _ = GT
    compare _ Fri = LT
    compare _ _ = EQ
```

* Ord should always agree with Eq i.e. if a == a' then compare a a' should be EQ

### Ord implies Eq

* Doing Ord a => a implies that a has an instance of Eq
* Normally want to use minimally sufficient typeclass so should use Eq is Ord is not needed

### Exercises: Will They Work?

1. Will work, will return 5
2. Will work, will return LT
3. Won't work because different datatypes, different definitions of Ord
4. Will work, will return False

## Enum

* Covers types that have known predecessors and successors
* e.g. `succ 4` returns 5
* `pred 'd'` returns 'c'
* can also use to build a list
* e.g. `enumFromTo 3 8` returns [3,4,5,6,7,8]
* `enumFromTo 'a' 'f'` returns "abcdef"
* `enumFromThenTo 1 10 100` returns [1,10,19,28,37,46,55,64,73,82,91,100]

## Show

* Show is used to return human readable strings of data
* creates strings it can return in terminal

```
class Show a where
    showsPrec :: Int -> a -> ShowS
    show :: a -> String
    showList :: [a] -> ShowS
```

### Printing and side effects

* Printing has effects outside of the scope of the statement
* Called a side effect
* Haskell seperates computations with effects from pure computations
* Haskell deals with side effects without adding anything to pure lambda calculus
* Type of print is: `print :: Show a => a -> IO ()` explicitly declares it has effects
* () is an empty tuple, called unit
* Value that represents nothing
* Function can't return nothing so it returns unit instead, meaning that the side effects are finished
  
### Working with Show

```
data Mood = Blah

instance Show Mood where
    show _ = "Blah"
```

* Now putting Blah into terminal outputs Blah
* Can usually just say `deriving Show`

## Read

* Read is opposite of Show, takes strings and turns them into things
* Can cause issues if the String is empty or if the text in the string can't easily be turned into the wanted data type

## Gimme more operations

* Sometimes need to constrain types with typeclasses to get the operations you need
* i.e `Num a => a -> a -> a` to get numeric operations
* Concrete types imply the typeclasses they provide
* So don't have to specify Num a because Int has an instance of Num
* Can ensure a function only does what you want it to do by only providing certain typeclasses
* i.e. could write a subset of Int that only has typeclasses you want to avoid inherent typeclasses of Int

## Chapter Exercises

### Multiple Choice

1. .
   1. c
   2.  c
   3.  a
   4.  c
   5.  a

### Does it typecheck?

See file

1. Doesn't typecheck, needs to derive Show in the type definition
2. Doesn't typecheck, needs to derive Eq
3. 
   1. Blah, Woot
   2. type error
   3. won't work, needs Ord
4. Will typecheck, contains Show typeclass

### What can we do?

1. Won't compile, types aren't declared
2. Will compile, types are declared
3. Will compile, Papu derives Eq and so do the types in its definition
4. Won't compile, Papu doesn't have instance of Ord

### Match the types

1. Will work as the second definition is parametrically polymorphic (wrong, doesn't work, literal 1 needs to be Num)
2. Won't work as Num doesn't account for the .0 a the end
3. Will work since Fractional contains definition for .0
4. Will work
5. Will work as simply puts constraint on a
6. Will work, will simply constrain a to Int
7. Won't work, output must be Int since myX is Int
8. Won't work, can't go up from Int to Num
9. Will work as Int has instance of typeclass Ord
10. Will work as type isn't specified for xs
11. Won't work since mySort explicitly states Char

### Type-Kwon-Do Two

see file

## Definitions

1. Typeclass inheritance is when a typeclass has a superclass. This is
a way of expressing that a typeclass requires another typeclass to
be available for a given type before you can write an instance.
```
class Num a => Fractional a where
(/) :: a -> a -> a
recip :: a -> a
fromRational :: Rational -> a
```
Here the typeclass Fractional inherits from Num. We could also
say that Num is a superclass of Fractional. The long and short of it
is that if you want to write an instance of Fractional for some
𝑎, that type 𝑎, must already have an instance of Num before you
may do so.
```
-- Even though in principle
-- this could work, it will fail because
-- Nada doesn't have a Num instance
newtype Nada =
Nada Double deriving (Eq, Show)
instance Fractional Nada where
(Nada x) / (Nada y) = Nada (x / y)
recip (Nada n) = Nada (recip n)
fromRational r = Nada (fromRational r)
Then if you try to load it:
No instance for (Num Nada)
arising from the superclasses
of an instance declaration
```
In the instance declaration for
‘Fractional Nada’
You need a Num instance first. Can’t write one that makes sense?
Then you’re not allowed to have a Fractional instance either.
Them’s the rules.
1. Effects are how we refer to observable actions programs may take
other than compute a value. If a function modifies some state
or interacts with the outside world in a manner that can be
observed, then we say it has an effect on the world.
3. IO is the type for values whose evaluation bears the possibility
of causing side effects, such as printing text, reading text input
from the user, reading or writing files, or connecting to remote
computers. This will be explained in much more depth in the
chapter on IO.
1. An instance is the definition of how a typeclass should work for
a given type. Instances are unique for a given combination of
typeclass and type.
5. In Haskell we have derived instances so that obvious or common
typeclasses, such as Eq, Enum, Ord, and Show can have the instances
generated based only on how the datatype is defined. This is
so programmers can make use of these conveniences without
writing the code themselves, over and over.