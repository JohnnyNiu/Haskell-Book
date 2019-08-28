# Basic Datatypes

## Types

* Like sets in maths
* Every value has some type
* Groups of values with commonalities share types

## Data declaration

* e.g. `data Bool = False | True`
* keyword data, capitalize name of type, logical disjunction for definition

### Exercises: Mood Swing

1. Mood
2. Blah, Woot
3. should be `changeMood :: Mood -> Mood`, Woot is not the type, Mood is
4. See file

## Numeric Types

* All numeric types have instances Num typeclass which gives operators like (+), (-) etc
  
### Integral

1. `Int`: fixed-precision integer like in other language
2. `Integer`: arbitrary size integer like in maths
3. Use Integer unless there is a hardware reason to use Int to avoid out of range errors

### Fractional

1. `Float`: single-precision floating point like in other languages
2. `Double`: same as above but double-precision
3. `Rational`: represented as a ratio of two integers
4. `Scientific`: arbitrary precision using scientific notation. coefficient is an Integer and exponent is an Int
5. Avoid Float unless reason to use it
6. Some operations always return Fractional such as (/) e.g. 4 / 2 returns 2.0
7. Fractional is typeclass that always required Num typeclass. Num is superclass of Fractional

### Comparing Values

* `(==) :: Eq a => a -> a -> Bool` equality operator using Eq typeclass for comparing if things are equal
* `(<) :: Ord a => a -> a -> Bool` comparison operator using Ord typeclass for all things that can be ordered
* Can use for letters and strings as well 
* e.g. `'a' == 'a'` is True
* `'a' > 'b'` is False
* `"Julie" > "Chris"` is True
* Can be used for lists where items use Ord
  
## Bool

* have `not`, `&&` and `||` for logical operators
  
### Exercises: Find the Mistakes

1. `not True && True` returns False
2. `not (x == 6)`
3. `(1 * 2) > 5` returns False
4. `"Merry" > "Happy"` returns True

### Conditionals

* no if statements, instead if expressions
* `if True then t else f`

```
 if CONDITION
then EXPRESSION_A
else EXPRESSION_B
```

## Tuples

* Let you store and pass around multiples values within a single value
* two-tuple or pair = (x, y)
* three-tuple = (x, y, z)
* number is known as arity
* values don't need to be same type
* two-tuple has `fst` and `snd` for returning each value
* `swap` reverses order of two-tuples

## Lists

* Elements must be of same type
* uses [ ] syntax
* number of values isn't static

## Chapter Exercises

1. `length :: [a] -> Integer` uses Int instead of Integer
2. * 5
   * 3
   * 2
   * 5
3. `length [1, 2, 3]` returns an Int not an Integer so will not evaluate to a Fractional
4. use `div` for integral division instead of /
5. Bool, returns True
6. Bool, returns False
7. * True
   * Won't compile, list contains different types
   * 5
   * False
   * won't compile because 9 is not Bool
8. See file for 8-10

### Correcting Syntax

1.
```
f xs = w \`x\` 
    where w = length xs
```   
2. `g x = x`
3. `f (a, b) = a`

### Match types
1. c
2. b
3. a
4. d

## Definitions
1. A tuple is an ordered grouping of values. In Haskell, you cannot
have a tuple with only one element, but there is a zero tuple also
called unit or (). The types of the elements of tuples are allowed
to vary, so you can have both (String, String) or (Integer, String).
Tuples in Haskell are the usual means of briefly carrying around
multiple values without giving that combination its own name.
2. A typeclass is a set of operations defined with respect to a polymorphic
type. When a type has an instance of a typeclass, values
of that type can be used in the standard operations defined for
that typeclass. In Haskell, typeclasses are unique pairings of
class and concrete instance. This means that if a given type 𝑎
has an instance of Eq, it has only one instance of Eq.
3. Data constructors in Haskell provide a means of creating values
that inhabit a given type. Data constructors in Haskell have a
type and can either be constant values (nullary) or take one or
more arguments, like functions. In the following example, Cat
is a nullary data constructor for Pet and Dog is a data constructor
that takes an argument:
-- Why name a cat?
-- They don't answer anyway.
type Name = String
data Pet = Cat | Dog Name
The data constructors have the following types:
Prelude> :t Cat
Cat :: Pet
Prelude> :t Dog
Dog :: Name -> Pet
4. Type constructors in Haskell are not values and can only be used in
type signatures. Just as data declarations generate data constructors
to create values that inhabit that type, data declarations
generate type constructors which can be used to denote that type.
CHAPTER 4. BECAUSE PIGS CAN’T FLY 114
In the above example, Pet is the type constructor. A guideline
for differentiating the two kinds of constructors is that type
constructors always go to the left of the = in a data declaration.
5. Data declarations define new datatypes in Haskell. Data declarations
always create a new type constructor, but may or may
not create new data constructors. Data declarations are how we
refer to the entire definition that begins with the data keyword.
6. A type alias is a way to refer to a type constructor or type constant
by an alternate name, usually to communicate something more
specific or for brevity.
type Name = String
-- creates a new type alias Name of the
-- type String *not* a data declaration,
-- just a type alias declaration
7. Arity is the number of arguments a function accepts. This notion
is a little slippery in Haskell as, due to currying, all functions are
1-arity and we handle accepting multiple arguments by nesting
functions.
8. Polymorphism in Haskell means being able to write code in terms
of values which may be one of several, or any, type. Polymorphism
in Haskell is either parametric or constrained. The identity
function, id, is an example of a parametrically polymorphic
function:
id :: a -> a
id x = x
Here id works for a value of any type because it doesn’t use any
information specific to a given type or set of types. Whereas,
the following function isEqual:
isEqual :: Eq a => a -> a -> Bool
isEqual x y = x == y
Is polymorphic, but constrained or bounded to the set of types
which have an instance of the Eq typeclass. The different kinds
of polymorphism will be discussed in greater detail in a later
chapter.
