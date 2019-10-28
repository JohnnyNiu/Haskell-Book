# Monoid, Semigroup

## Algebras

* Refers to some operations and the set they operate over
* Less about particular data and more about the general rules of use
* Can be implemented with typeclasses that define the set of operations
* Set is tht type the operations are for
* Probably had monoid patterns in code without realizing it

## Monoid

* *A monoid is a binary associative operation with an identity*
* Binary as in there are two of something
* Associative meaning that the operation must satisfy associativity
* Operation = function
* Identity means that there's some value that when combined with a second value, always returns the second value

```haskell
-- definition of mappend
mappend [1, 2, 3] [4, 5, 6]
[1,2,3,4,5,6]

-- identity value for mappend
mappend [1..5] [] = [1..5]
mappend [] [1..5] = [1..5]
```

* In short monoid is the typeclass that generalizes the properties of associativity and identity

## How Monoid is defined in Haskell

```haskell
class Monoid m where
    mempty :: m
    mappend :: m -> m -> m
    mconcat :: [m] -> m
    mconcat = foldr mappend mempty
```

* mappend is how any two values of a type can be joined together
* mempty is the identity value for mappend

## Examples of using Monoid

* Monoids are already familiar, used a lot
* Best way to understand is to look at familiar ones

### List

* list is a common type with an instance of monoid

```haskell
mappend [1, 2, 3] [4, 5, 6]
[1,2,3,4,5,6]

mconcat [[1..3], [4..6]]
[1,2,3,4,5,6]

mappend "Trout" " goes well with garlic"
"Trout goes well with garlic"
```

* which is exactly the same as

```haskell
(++) [1, 2, 3] [4, 5, 6]
[1,2,3,4,5,6]

foldr (++) [] [[1..3], [4..6]]
[1,2,3,4,5,6]

(++) "Trout" " goes well with garlic"
"Trout goes well with garlic"

-- therefore

foldr mappend mempty [[1..3], [4..6]]
[1,2,3,4,5,6]
```

* this becomes clear looking at the defintion of Monoid for lists

```haskell
instance Monoid [a] where
    mempty = []
    mappend = (++)
```

## Why Integer doesn't have a Monoid

* None of the numeric types have an instance of Monoid
* But numbers have monoidal operations
* In maths the monoid of numbers is summation
* No clear reason why it can't be multiplication which is also monoidal
* Each type should only have one instance for a given typeclass, not two
* mappend won't work for Integer because it's ambiguous
* instead, there's the Sum and Product newtypes to wrap numeric values
* then signals which instance of Monoid we want
* newtypes are bult into Data.Monoid module

```haskell
mappend (Sum 1) (Sum 5)
Sum {getSum = 6}

mappend (Product 5) (Product 5)
Product {getProduct = 25}

mappend (Sum 4.5) (Sum 3.4)
Sum {getSum = 7.9}
```

* can be used with values that aren't integral
* work for all types that have instances of Num
* *Integers form a monoid under summation and multiplication*
* Can also say that lists form a monoid under concatenation
* Numbers aren't the only set that have more than one possible monoid
* Usually use newtypes to separate monoidal behaviours

### Why newtype?

* newtype constrains datatype to have single unary data constructor
* guarantees no additional runtime overhead

#### (verbatim) In summary, why you might use newtype 
1. To signal intent: using newtype makes it clear that you only intend
for it to be a wrapper for the underlying type. The newtype
cannot eventually grow into a more complicated sum or product
type, while a normal datatype can.
2. To improve type safety: avoid mixing up many values of the
same representation, such as Text or Integer.
3. To add different typeclass instances to a type that is otherwise
unchanged representationally, such as with Sum and Product.

### More on Sum and Product

```haskell
newtype Sum a = Sum {getSum :: a}
...some instances elided...
instance Num a => Monoid (Sum a)

newtype Product a = Product {getProduct :: a}
...some instances elided...
instance Num a => Monoid (Product a)
```

* the infix operator for mappend:

```haskell
(<>) :: Monoid m => m -> m -> m

(Sum 8) <> (Sum 9)
Sum {getSum = 17}

mappend mempty Sum 9
Sum {getSum = 9}

-- can't do
mappend (Sum 8) (Sum 9) (Sum 10)
-- have to nest
mappend (Sum 1) (mappend (Sum 2) (Sum 3))
Sum {getSum = 6}
-- but using infix instead
Sum 1 <> Sum 1 <> Sum 1
Sum {getSum = 3}
-- can use mconcat using a list
mconcat [Sum 8, Sum 9, Sum 10]
Sum {getSum = 27}
```

* special syntax of Sum and Product mean a record field accessor is needed to unwrap the value:

```haskell
getSum $ mappend (Sum 1) (Sum 1)
2

getProduct $ mappend (Product 5) (Product 5)
25

getSum $ mconcat [(Sum 5), (Sum 6), (Sum 7)]
18
```

## Why Bother?

* Monoids are common and are a nice abstraction to work with when there are multiple monoids in a project
* having principled laws means you can combine monoidal operations safely
* common use it to structure and describe common modes of processing data
* e.g. in an API or in concurrent programming
* Monoids strongly associated with folding

```haskell
foldr mappend mempty ([2, 4, 6] :: [Product Int])
Product {getProduct = 48}

foldr mappend mempty ([2, 4, 6] :: [Sum Int])
Sum {getSum = 12}

foldr mappend mempty ["blah", "woot"]
"blahwoot"
```

## Laws

* We want our programs to be correct whenever possible
* proofs are programs, programs are proofs
* programs should compose well, be easy to understand and have predictable behvaiour
* Algebras are defined by their laws and are useful principally for their laws. Laws make up what algebras are
* Monoid instances must abide by these laws:

```haskell
-- left identity
mappend mempty x = x

mappend mempty (Sum 1)
Sum {getSum = 1}

-- right identity
mappend x mempty = x

mappend (Sum 1) mempty
Sum {getSum = 1}

-- associativity
mappend x (mappend y z) = mappend (mappend x y) z

(Sum 1) <> (Sum 2 <> Sum 3)
Sum {getSum = 6}

(Sum 1 <> Sum 2) <> (Sum 3)
Sum {getSum = 6}

-- concat
mconcat = foldr mappend mempty

mconcat [Sum 1, Sum 2, Sum 3]
Sum {getSum = 6}

foldr mappend mempty [Sum 1, Sum 2, Sum 3]
Sum {getSum = 6}
```

* Now the same laws demonstrated but using the List Monoid

```haskell
-- mempty is []
-- mappend is (++)

-- left identity
mappend mempty [1, 2, 3]
[1,2,3]

-- right identity
mappend [1, 2, 3] mempty
[1,2,3]

-- associativity
[1] <> ([2] <> [3])
[1,2,3]

([1] <> [2]) <> [3]
[1,2,3]

-- mconcat ~ foldr mappend mempty
mconcat [[1], [2], [3]]
[1,2,3]

foldr mappend mempty [[1], [2], [3]]
[1,2,3]

concat [[1], [2], [3]]
[1,2,3]
```

* important point to make is that you are guaranteed these laws even if you don't know what particular Monoid you'll be working with

## Different intance, same representation

* many datatypes have more than one valid monoid, making it different to other typeclasses
* when there's more than one, useful to use newtypes to differentiate them
* for some datatypes the meaning of append can be unclear
* in such cases, less about combining values and more about finding a summary value for the set
* better to think about mappend this way since it is more general
* Bool has two possible monoids, conjunction or disjunction
* newtypes for these are `All` and `Any`

```haskell
All True <> All True
All {getAll = True}

All True <> All False
All {getAll = False}

Any True <> Any False
Any {getAny = True}

Any False <> Any False
Any {getAny = False}
```

* this doesn't really feel like appending in a strict sense
* more of a reducing operation
* Maybe has more than two possible Monoids
* First and Last allow you to choose which Just value is returned in cases with moer than one Just value

```haskell
-- First returns the first or leftmost non-Nothing value:
First (Just 1) `mappend` First (Just 2)
First {getFirst = Just 1}

-- Last returns the last or rightmost non-Nothing value:
Last (Just 1) `mappend` Last (Just 2)
Last {getLast = Just 2}
```

* will always succeed in returning something if there are any non-Nothing values
* if there is only Nothing then they will return Nothing

## Reusing algebras by asking for algebras

* The other Monoid instance for Maybe is not concerned with picking a value out of a set, it's about combining a values in the Maybe a type

```haskell
instance Monoid b => Monoid (a -> b)

instance (Monoid a, Monoid b) => Monoid (a, b)

instance (Monoid a, Monoid b, Monoid c) => Monoid (a, b, c)
```

* point of these is that you get a Monoid for a larger type by reusing the Monoid instances of the types that represent components of the larger type
* Must provide a Monoid instance for the a in Maybe a even though not all type definitions use the a, i.e. Nothing doesn't contain type a

### Exercise: Optional Monoid

see file

### Associativity

* Says you can group your arguments differently and results will be the same
* not as strong a property as an operation that is commutative
* commutative means you can reorder the arguments and get the same result
* (++) is not commutative

### Identity

* Identity turns the operation into the identity function
* e.g. 0 for addition, 1 for multiplication, [] for lists

### The problem of orphan instances

* orphan instance is when an instance is defined for a datatype and typeclass but not in the same module as either the declaration of the typeclass or the datatype
* have to use newtype to avoid this by taking ownership of the type under a new name
* can be a problem when multiple instances for a typeclass are defined in different modules
* can lead to conflicts if both are imported or unpredictable behaviour if only one is imported

1. You defined the type but not the typeclass? Put the instance in
the same module as the type so that the type cannot be imported
without its instances.
2. You defined the typeclass but not the type? Put the instance in
the same module as the typeclass definition so that the typeclass
cannot be imported without its instances.
3. Neither the type nor the typeclass are yours? Define your own
newtype wrapping the original type and now you’ve got a type
that “belongs” to you for which you can rightly define typeclass
instances. There are means of making this less annoying which
we’ll discuss later.

## Madness

see file

## Better living through QuickCheck

### Validating associativity with QuickCheck

* can check the associativity of simple arithmetic expressions by asserting equality between different parentheses

```haskell
1 + (2 + 3) == (1 + 2) + 3

4 * (5 * 6) == (4 * 5) * 6
```

* doesn't mean associativity holds for any inputs
* can use lambda calculus for this:

```haskell
\ a b c -> a + (b + c) == (a + b) + c

\ a b c -> a * (b * c) == (a * b) * c
```

* but it's also possible to abstract the associativity of a function:

```haskell
\ f a b c -> f a (f b c) == f (f a b) c

-- or infix
\ (<>) a b c -> a <> (b <> c) == (a <> b) <> c
```

* bottom one demonstrates binding infix names as function arguments

```haskell
asc :: Eq a => (a -> a -> a) -> a -> a -> a -> Bool
asc (<>) a b c = a <> (b <> c) == (a <> b) <> c
```

* must turn this into a property to be able to QuickCheck

```haskell
monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

-- adding concrete types for value generation:
type S = String
type B = Bool
quickCheck (monoidAssoc :: S -> S -> S -> B)
```

### Testing left and right identity

```haskell
monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdenity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a
```

### Testing QuickCheck's patience

* an example where QuickCheck catches an invalid Monoid
* see file
* turns out it passes QuickCheck for associativity but not for identity
* mappend always returns Fools, which means it's not behaving like an identity
* it's more like a black box

### Exercise: Maybe Another Monoid

see file

## Semigroup

* Difference between a Monoid and a Semigroup is no longer needing an identity
* operation remains binary and associative

```haskell
class Semigroup a where
    (<>) :: a -> a -> a

-- left with this law
(a <> b) <> c = a <> (b <> c)
```

* usually joins two things together, but weaker than monoid
* not yet part of Prelude, must be imported using Data.Semigroup

### NonEmpty, a useful datatype

* useful type that can't have a monoid instance but does have a semigroup instance
* can never be an empty list

```haskell
data NonEmpty a = a :| [a]
    deriving (Eq, Ord, Show)
```

* `:|` is an infix data constructor taking two arguments
* product of a and [a]
* guarantees always having one value of type a
* `:|` is non-alphanumeric and therefore infix by default
* alphanumeric names are prefix by default
* could also write NonEmpty like this:

```haskell
newtype NonEmpty a =
    NonEmpty (a, [a])
    deriving (Eq, Ord, Show)
```

* has no identity value by design therefore not possible to have a Monoid
* no empty list to function as an identity for an operation over NonEmpty
* lists can still be concatenated so there's still a need for semigroup
  
## Strength can be weakness

* strength of an algebra usually means the number of operations it provides
* more you can do with an instance without needing to know concrete type
* don't want to make algebras too big because it might exclude some datatype that would work if some operation or law were removed
* See this happening with NonEmpty, necessitating the use of semigroup
* Monoid is stronger because it has a superset of the operations of semigroup
* Inverse relationship between the number of operations and the number of datatypes that can provide an instance of that algebra

```haskell
id :: a -> a
```

* id has infinite number of types but only one operation
* inc a only takes numeric datatypes buit provides all numeric operations:

```haskell
inc :: Num a => a -> a
```

* inc has 7 operations but limited number of datatypes
* somethingInt has many more operations but only takes one datatype:

```haskell
somethingInt :: Int -> Int
```