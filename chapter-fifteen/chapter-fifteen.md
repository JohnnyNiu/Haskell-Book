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