# Functor

* Another algebra, more powerful than Monoid
* is a pattern of mapping over structure, not limited to lists

## What's a functor?

* Applies a function to a structure that you don't want to alter
* Changes the value inside the structure but leaves the structure alone
* Applies to many kinds of structures, but most often lists

```haskell
class Functor f where
    fmap :: (a -> b) -> f a -> f b
```

* Typeclass just like with Monoid except the type being used is notated by f which is used to represent a functional type
* `a -> b` is any function of the type, could even be `a -> a`
* `f a` is a Functor f that takes type argument a
* means that f is a type that has an instance of Functor
* return type is `f b` f is the same as f from above, but b can be different type from a

## There's a whole lot of fmap goin' round

* with lists , fmap does the same thing as map

```haskell
map (\x -> x > 3) [1..6]
[False,False,False,True,True,True]

fmap (\x -> x > 3) [1..6]
[False,False,False,True,True,True]
```

* List is a type that implements Functor
* map only works on List, but other types implement Functor so fmap works

```haskell
fmap (+1) (Just 1)
Just 2

fmap (10/) (4, 5)
(4,2.0)

let rca = Right "Chris Allen"
fmap (++ ", Esq.") rca
"Chris Allen, Esq."
```

* How fmap specializes to different types:

```haskell
type E e = Either e
type C e = Constant e
type I = Identity

fmap :: (a -> b) -> f a -> f b
     :: (a -> b) -> [ ] a -> [ ] b
     :: (a -> b) -> Maybe a -> Maybe b
     :: (a -> b) -> E e a -> E e b
     :: (a -> b) -> (e,) a -> (e,) b
     :: (a -> b) -> I a -> I b
     :: (a -> b) -> C e a -> C e b
```

* the e signifies that fmap ignores the first argument, will be explained later

## Let's talk about f, baby

* f has the kind `* -> *`
* a and b in the Functor definition must have the kind *, as function arguments must be fully applied
* therefore f can only be `* -> *` so that a and b can be applied to it

### Shining start coming into view

```haskell
class Sumthin a where
    s :: a -> a
-- argument and result are both a, so a has kind *

class Else where
    e :: b -> f (g a b c)
-- b has kind *
-- f takes one argument, (g a b c), therefore has kind * -> *
-- g takes three argument, (a b c), therefore has kind *->*->*->*

class Biffy where
slayer :: e a b
    -> (a -> c)
    -> (b -> d)
    -> e c d
-- e takes two arguments, so has kind *->*->*
-- a is argument to function therefore *, same with c

class Impish v where
    impossibleKind :: v -> v a

class AlsoImp v where
    nope :: v a -> v
-- neither of these work, v has a type argument in one instance and not in the other, compiler will refuse to accept this
```

### Exercises: Be Kind

1. *
2. `b :: *->*`, `T :: *->*`
3. `*->*->*`

### A shining for for you to see

* see example FixMePls
* won't compile due to not having type arguments
* FixMePls has wrong kindedness
* to see why this is nonsensical:

```haskell
fmap :: Functor f => (a -> b) -> f a -> f b

(a -> b) -> FixMePls a -> FixMePls b
-- FixMePls doesn't take type arguments, so it's equivalent to:
(FixMePls -> FixMePls) -> FixMePls -> FixMePls

-- using polymorphism to abstract out the type:
(a -> b) -> a -> b
-- is the same as:
($) :: (a -> b) -> a -> b
-- which is the definition of function application
```

### Functor is function application

* More accurately, fmap is a specific sort of function application
* `fmap :: Functor f => (a -> b) -> f a -> f b`
* Also an infix operator for fmap
* `(<$>) :: Functor f => (a -> b) -> f a -> f b`
* There is a pattern between `<$>` and `$`:

```haskell
(<$>) :: Functor f
    => (a -> b) -> f a -> f b
($) :: (a -> b) -> a -> b
```

* Functor is just function application "over" a structure, while ignoring the structure itself and leaving it untouched

### A shining star for you to see what your f can truly be

* Can add a type constructor to FixMePls, see examples file
* Now it compiles
* See fixed Functor instance for FixMePls in example
* lining it up with the definition of fmap:

```haskell
fmap :: Functor f
    => (a -> b) ->     f a     -> f b
fmap       f       (Pls a) = Pls (f a)   
```

* Note, that the type passed to Functor is `FixMePls` and not `FixMePls a` because `FixMePls a` has kind *

## Functor Laws

* Instances of Functor must abide by two basic laws
* Understanding these is essential for understanding Functor and writing good typeclasses

### Identity

`fmap id == id`

* If fmap is applied to identity function, should be the same as passing the value to identity
* If the `(a -> b)` function doesn't return a new value, then nothing should change as the structure is left alone

### Composition

`fmap (f . g) == fmap f . fmap g`

* fmapping composed functions should be the same as fmapping then composing:

```haskell
fmap ((+1) . (*2)) [1..5]
[3,5,7,9,11]

fmap (+1) . fmap (*2) $ [1..5]
[3,5,7,9,11]
```

* If that doesn't work, your Functor is broken

### Structure preservation

* Both laws are based on the essential rule that functors are structure preserving
* Using the base definition of fmap, we don't know what f is, all we know if that it must be a type that takes an argument, because that is its structure

## The Good, the Bad, and the Ugly

```haskell
-- only one data constructor that could be fmapped over
data WhoCares a =
ItDoesnt
    | Matter a
    | WhatThisIsCalled
    deriving (Eq, Show)

-- law abiding instance
instance Functor WhoCares where
    fmap _ ItDoesnt = ItDoesnt
    fmap _ WhatThisIsCalled = WhatThisIsCalled
    fmap f (Matter a) = Matter (f a)

-- breaks the law due to violating identity
instance Functor WhoCares where
    fmap _ ItDoesnt = WhatThisIsCalled
    fmap f WhatThisIsCalled = ItDoesnt
    fmap f (Matter a) = Matter (f a)

fmap id ItDoesnt
WhatThisIsCalled

fmap id WhatThisIsCalled
ItDoesnt

fmap id ItDoesnt == id ItDoesnt
False

fmap id WhatThisIsCalled == WhatThisIsCalled
False
```

### The law won

* There is a way to change the structure and the values
* It's called a function
* The point of Functor is that it's a special application where the structure is untouched
* Therefore we must stick to the laws to preserve this

### Composition should just work

* Composition law actually logically follows from identity law, but is important so special attention is applied
* Goal is to preserve composability and prevent unpleasant surprises

```haskell
data CountingBad a =
    Heisenberg Int a
    deriving (Eq, Show)

-- super NOT okay
instance Functor CountingBad where
    fmap f (Heisenberg n a) =
-- (a -> b) f a =
        Heisenberg (n+1) (f a)
-- f b
```

* Heisenberg has two arguments but CountingBad has one
* Doesn't jive well with the definiton of fmap
* Not clear what part of fmap n refers to

```haskell
u = "Uncle"
let oneWhoKnocks = Heisenberg 0 u
fmap (++" Jesse") oneWhoKnocks
Heisenberg 1 "Uncle Jesse"

f = ((++" Jesse").(++" lol"))
fmap f oneWhoKnocks
Heisenberg 1 "Uncle lol Jesse"
```

* That composes properly but what if we compose them separately:

```haskell
j = (++ " Jesse")
l = (++ " lol")
fmap j . fmap l $ oneWhoKnocks
Heisenberg 2 "Uncle lol Jesse"
```

* Not equivalent to the above composition due to subsequent application increasing n
* This will obey the law:
 
```haskell
instance Functor CountingGood where
    fmap f (Heisenberg n a) =
        Heisenberg (n) (f a)
```

## Commonly used functors

```haskell
:t const
const :: a -> b -> a

replaceWithP = const 'p'
replaceWithP 10000
'p'
replaceWithP "woohoo"
'p'
replaceWithP (Just 10)
'p'

-- data Maybe a = Nothing | Just a

fmap replaceWithP (Just 10)
Just 'p'
fmap replaceWithP Nothing
Nothing

-- data [] a = [] | a : [a]

fmap replaceWithP [1, 2, 3, 4, 5]
"ppppp"
map replaceWithP "Ave"
"ppp"
fmap (+1) []
[]
fmap replaceWithP []
""

-- data (,) a b = (,) a b

fmap replaceWithP (10, 20)
(10,'p')
fmap replaceWithP (10, "woo")
(10,'p')

negate 10
-10
tossEmOne = fmap (+1) negate
tossEmOne 10
-9
tossEmOne (-10)
11

tossEmOne' = (+1) . negate
tossEmOne' 10
-9
tossEmOne' (-10)
11
```

### The functors are stacked and that's a fact

```haskell
-- lms ~ List (Maybe (String))
n = Nothing
w = Just "woohoo"
ave = Just "Ave"
lms = [ave, n, w]

replaceWithP lms
'p'
fmap replaceWithP lms
"ppp"

-- lms involves two datatypes that have Functor instances
-- fmap is not limited to the outermost datatype

(fmap . fmap) replaceWithP lms
[Just 'p',Nothing,Just 'p']

tripFmap = fmap . fmap . fmap
tripFmap replaceWithP lms
[Just "ppp",Nothing,Just "pppppp"]

replaceWithP lms
'p'
:t replaceWithP lms
replaceWithP lms :: Char

-- In:
replaceWithP lms
-- replaceWithP's input type is:
List (Maybe String)
-- The output type is Char
-- So applying
replaceWithP
-- to
lms
-- accomplishes
List (Maybe String) -> Char

fmap replaceWithP lms
"ppp"
-- fmap is going to leave the list
-- structure intact around our result:
:t fmap replaceWithP lms
fmap replaceWithP lms :: [Char]

-- In:
fmap replaceWithP lms
-- replaceWithP's input type is:
Maybe String
-- The output type is Char
-- So applying
fmap replaceWithP
-- to
lms
-- accomplishes:
List (Maybe String) -> List Char
-- List Char ~ String
```

#### What if we lift twice?

```haskell
(fmap . fmap) replaceWithP lms
[Just 'p',Nothing,Just 'p']

:t (fmap . fmap) replaceWithP lms
(fmap . fmap) replaceWithP lms :: [Maybe Char]

-- In:
(fmap . fmap) replaceWithP lms
-- replaceWithP's input type is:
-- String aka List Char or [Char]
-- The output type is Char
-- So applying
(fmap . fmap) replaceWithP
-- to
lms
-- accomplishes
List (Maybe String) -> List (Maybe Char)
```

#### Wait, how does that even typecheck?

* do exercise later

#### Lifte me baby one more time

```haskell
tripFmap = fmap . fmap . fmap

tripFmap replaceWithP lms
[Just "ppp",Nothing,Just "pppppp"]

:t tripFmap replaceWithP lms
(fmap . fmap . fmap) replaceWithP lms :: [Maybe [Char]]

-- In
(fmap . fmap . fmap) replaceWithP lms
-- replaceWithP's input type is:
-- Char
-- because we lifted over
-- the [] of [Char]
-- The output type is Char
-- So applying
(fmap . fmap . fmap) replaceWithP
-- to
lms
-- accomplishes
List (Maybe String) -> List (Maybe String)
```

#### The real type of thing going down

* Summarizing the pattern above:

```haskell
fmap replaceWithP lms
"ppp"

(fmap . fmap) replaceWithP lms
[Just 'p',Nothing,Just 'p']

tripFmap = fmap . fmap . fmap
tripFmap replaceWithP lms
[Just "ppp",Nothing,Just "pppppp"]
```

* Summarizing the types

```haskell
replaceWithP' :: [Maybe [Char]] -> Char
replaceWithP' = replaceWithP

[Maybe [Char]] -> [Char]
[Maybe [Char]] -> [Maybe Char]
[Maybe [Char]] -> [Maybe [Char]]
```

#### Get on up and down

```haskell
-- lmls ~ List (Maybe (List String))

ha = Just ["Ha", "Ha"]
lmls = [ha, Nothing, Just []]

(fmap . fmap) replaceWithP lmls
[Just 'p', Nothing, Just 'p']

tripFmap replaceWithP lmls
[Just "pp", Nothing, Just ""]

(tripFmap.fmap) replaceWithP lmls
[Just ["pp","pp"], Nothing, Just []]
```

#### One more round for the P-Funkshun

* see example replaceWithP

### Exercises: Heavy Lifting

1. see exercises

## Transforming the unapplied type

* When fmapping over a tuple it only transforms the second argument
* Same with Either

```haskell
data Two a b = Two a b deriving (Eq, Show)

data Or a b = First a | Second b deriving (Eq, Show)
```

* These examples are based on the form of (,) and Either
* Both are kinded `*->*->*`
* Can't apply Functor directly to them
* Must partially apply the types to create an instance of Functor

```Haskell
instance Functor (Two a) where
    fmap = undefined

instance Functor (Or a) where
    fmap = undefined
```

* This will pass the typechecker but fmap needs to be implemented
* This won't work, the f in Functor f doesn't represent Two, it represents Two a

```haskell
instance Functor (Two a) where
    fmap f (Two a b) = Two $ (f a) (f b)
```

* Instead we have to leave out the leftmost value alone and apply the function to the rightmost

```haskell
instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)
```

* Or works a bit differently because it's one or the other value
* Concept is the same though, function only applies to the rightmost

```haskell
instance Functor (Or a) where
    fmap _ (First a) = First a
    fmap f (Second b) = Second (f b)
```

## QuickChecking Functor instances

```haskell
fmap id = id
fmap (p . q) = (fmap p) . (fmap q)
```

* Those are the laws for Functors, which can be turned into quickcheck properties:

```haskell
functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)
```

### Making QuickCheck generate functions too

```haskell
import Test.QuickCheck
import Test.QuickCheck.Function

functorCompose' :: (Eq (f c), Functor f) =>
                    f a -> Fun a b -> Fun b c -> Bool
functorCompose' x (Fun _ f) (Fun _ g) =
    (fmap (g . f) x) == (fmap g . fmap f $ x)
```

* Needs a new module from Test.QuickCheck
* Pattern matches ignores the first part of the function type, the second part is the function itself
* Can't print Fun values so verboseCheck won't work

## Exercises: Instance of Func

* See exercises file

## Ignoring Possibilities

* Can take advantage of the fact that fmap ignores the leftmost case in Maybe and Either
* Leftmost will usually will be the error case

### Maybe

```haskell
incIfJust :: Num a => Maybe a -> Maybe a
incIfJust (Just n) = Just $ n + 1
incIfJust Nothing = Nothing

showIfJust :: Show a => Maybe a -> Maybe String
showIfJust (Just s) = Just $ show s
showIfJust Nothing = Nothing
```

* has redundant code and can be cleaned up using fmap

```haskell
incMaybe :: Num a => Maybe a -> Maybe a
incMaybe m = fmap (+1) m

showMaybe :: Show a => Maybe a -> Maybe String
showMaybe s = fmap show s
```

* still works and is a bit tidier than the one above
* but can be abstracted a bit more
* can eta-reduce them, rewriting without arguments

```haskell
incMaybe'' :: Num a => Maybe a -> Maybe a
incMaybe'' = fmap (+1)

showMaybe'' :: Show a => Maybe a -> Maybe String
showMaybe'' = fmap show
```

* can also rewrite them more generically, using the most abstract types possible

```haskell
liftedInc :: (Functor f, Num b) => f b -> f b
liftedInc = fmap (+1)

liftedShow :: (Functor f, Show a) => f a -> f String
liftedShow = fmap show
```

#### Exercise: Possibly

* see exercises

### Either

* Following the same pattern as for Maybe:

```haskell
incIfRight :: Num a => Either e a -> Either e a
incIfRight (Right n) = Right $ n + 1
incIfRight (Left e) = Left e

showIfRight :: Show a => Either e a -> Either e String
showIfRight (Right s) = Right $ show s
showIfRight (Left e) = Left e

-- simplifying

incEither :: Num a => Either e a -> Either e a
incEither m = fmap (+1) m

showEither :: Show a => Either e a -> Either e String
showEither s = fmap show s

-- eta-contract

incEither' :: Num a => Either e a -> Either e a
incEither' = fmap (+1)

showEither' :: Show a => Either e a -> Either e String
showEither' = fmap show

-- generic

liftedInc :: (Functor f, Num b) => f b -> f b
liftedInc = fmap (+1)

liftedShow :: (Functor f, Show a) => f a -> f String
liftedShow = fmap show
```

#### Short exercise

1. see exercises
2. can't just pass the second argument of the type, have to apply the first argument before the second

## A somewhat surprising functor

* datatype called Const or Constant based on which library is being used
* The function const:

```haskell
:t const
const :: a -> b -> a

a = const 1
a 1
1

a 2
1

a 3
1

a "blah"
1

a id
1
```

* Introducing the Constant datatype:

```haskell
newtype Constant a b =
    Constant { getConstant :: a }
    deriving (Eq, Show)
```

* parameter b is a phantom type, doesn't appear anywhere inside the constructor

```haskell
Constant 2
Constant {getConstant = 2}
```

* even though b is a phantom type, Constant still has kind `*->*->*`
* So Constant a has a Functor instance but Constant doesn't
* On surface looks like identity

```haskell
instance Functor (Constant m) where
    fmap _ (Constant v) = Constant v

const 2 (getConstant (Constant 3))
2

fmap (const 2) (Constant 3)
Constant {getConstant = 3}

gc = getConstant
c = Constant 3

gc $ fmap (const 2) c
3

gc $ fmap (const "blah") c
3
```

* Because Constant is partially applied to the first parameter, it returns the second parameter
* The first parameter however, is the constant that would usually be returned
* So it returns the second argument, which doesn't make a whole lot of sense semantically

## More structure, more functors

* structure of types may require a Functor instance for an intermediate type

```haskell
data Wrap f a = Wrap (f a) deriving (Eq, Show)

-- this doesn't work because a is already an argument to a function
instance Functor (Wrap f) where
    fmap f (Wrap fa) = Wrap (f fa)

-- this doesn't work because we don't know the type of f, so it may not have a Functor instance
instance Functor (Wrap f) where
    fmap f (Wrap fa) = Wrap (fmap f fa)

-- this constrains f to a type that has a Functor instance
instance Functor f => Functor (Wrap f) where
fmap f (Wrap fa) = Wrap (fmap f fa)
```
