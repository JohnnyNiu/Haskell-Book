# Algebraic Datatypes

* type can be thought of as an enumeration of constructors that have zero or more arguments
* chapter will focus on this adn creating new datatypes

## Data Declarations Review

* creating custom datatypes for structuring and describing data helps analyze problem and focus on modelling the domain
* `data [] a = [ ] | a : [a]` `data Bool = False | True`
* Bool arguments are nullary constructors, take no arguments
* pipe indicates sum type, logical disjunction
* Bool enumeration of two possible constructors taking zero arguments
* [] two possible constructors, one takes two arguments
* sum type has more than one constructor

## Data and Type Constructors

* Type constructors are used only at the type level, in type signatures andtypeclass declarations and instances
* Types are static and resolve at compile time
* Data constructors construct the values at term level, values you can interact with at runtime.
* Type and data constructors that take no arguments are technically constants, since they store a fixed type and amount of data
* Not constructed in any meaningful sense
* Sometimes constructors need to take arguments, which behave like function arguments
* `data Trivial = Trivial'` vs `data UnaryTypeCon a = UnaryValueCon a`
* Trivial' is like a constant value, that exists at the term level
* UnaryTypeCon is type constructor of one argument, awaits a type constant to be applied to
* UnaryValueCon is data constructor of one argument, awaits a value to be applied to. 
* Doesn't perform an operation like a function, more akin to a box to put values in. But that is not entirely accurate
* Trivial will always have the concrete value Trivial' but UnaryValueCon could have different concrete values based on the type of a

## Type constructors and kinds

* Kinds are the types of types, or types one level up
* represented with *
* something is a fully applied, concrete type when represented as *
* function still to be applied is represented as * -> *
* can query kind with `:kind` or `:k`
* `:k Bool` == `Bool :: *`, `:k [Int]` == `[Int] :: *`, `:k []` == `[] :: * -> *`
* last one not fully applied because it needs a concrete type
* that's what the constructor of type constructor refers to

## Data constructors and values

```
data PugType = PugData
data HuskyType a = HuskyData
data DogueDeBordeaux doge = DogueDeBordeaux doge
```

* PugType is the type constructor but takes no arguments so can think of it as being type constant
* PugData only data constructor for PugType. Also a constant value 
* HuskyType type constructor that takes single polymorphic argument
* HuskyData data constructor for HuskyType, argument for HuskyType does not appear so HuskyData is contant. type argument a is called a phantom, has no witness
* DogueDeBordeaux also type constructor with single argument, called doge
* Data constructor has the same name, but is not the same thing. Has the same type variable doge so must agree with the argument from the type constructor
* Here are those types with concrete values:

```
myPug = PugData :: PugType

myHusky :: HuskyType a
myHusky = HuskyData

myOtherHusky :: Num a => HuskyType a
myOtherHusky = HuskyData

myDoge :: DogueDeBordeaux Int
myDoge = DogueDeBordeaux 10

-- this doesn't work because 10 is not of type String:
badDoge :: DogueDeBordeaux String
badDoge = DogueDeBordeaux 10
```

### Exercises: Dog Types

1. type constructor
2. `* -> *`
3. `*`
4. `Doggies Int` -- compiler gives `Num a => Doggies a`
5. `Doggies Integer`
6. `Doggies String` -- compiler says `Doggies [Char]`, which is synonym
7. Both
8. `doge -> DogueDeBordeaux doge`
9. `DogueDeBordeaux [Char]`

## What's a type and what's data?

* types are static and resolve at compile time
* types are known before runtime via declaration or inference, making them static
* information about types doesn't persist to run time
* data is worked with at runtime
* sometimes data constructors depend on other types which must be in scope
* e.g. `data Price = Price Integer deriving (Eq, Show)` needs Integer to generate Price values


```
data Manufacturer = Mini | Mazda | Tata deriving (Eq, Show)

data Airline = PapuAir | CatapultsR'Us | TakeYourChancesUnited deriving (Eq, Show)
```

* each are sum types and have 3 data constructors
* now can construct another type:
* `data Vehicle = Car Manufacturer Price | Plane Airline deriving (Eq, Show)`
* has two data constructors, Car constructor need Manufacturer and Price values, Plane constructor needs Airline value
* usually want to include `deriving Show` for custom datatypes to allow printing your data
* deriving Eq lets you derive equality operations for most datatypes

### Exercises: Vehicles

1. Vehicle
2. see file
3. ^
4. error because plane doesn't have manufacturer type
5. see file

## Data Constructor Arities

* Data constructor with no arguments is nullary, and are constant values
* data constructors with one argument are unary
* more than one argument are called products

```
-- nullary
data Example0 = Example0
    deriving (Eq, Show)

-- unary
data Example1 = Example1 Int
    deriving (Eq, Show)

-- product of Int and String
data Example2 = Example2 Int String
    deriving (Eq, Show)
```

## What makes these datatypes algebraic?

* Called algebraic because their patterns can be describes using the operations sum and product
* Can be demonstrated in terms of cardinality, number of possible values it defines
* Can be between 0 and infinite
* Cardinality of Bool is 2, cardinality of Int8 is 256

### Exercises: cardinality

1. 1
2. 3
3. 65535
4. cardinality of Integer is infinite, cardinality of Int is enormous
5. 256 is the eighth power of two

### Exercises: for example

`data Example = MakeExample deriving Show`

1. Example; error because no data constructor called example
2. yes, shows instance of Show
3. `Int -> Example`


## newtype

* newtype defines types that can only have a single unary data constructor
* cannot be a product or sum type or have nullary constructors
* no runtime overhead

```
tooManyGoats :: Int -> Bool
tooManyGoats n = n > 42

newtype Goats = Goats Int deriving (Eq, Show)
newtype Cows = Cows Int deriving (Eq, Show)

tooManyGoats :: Goats -> Bool
tooManyGoats (Goats n) = n > 42

tooManyGoats (Goats 43) == True
tooManyGoats (Cows 43) == error
```

* this pattern is safer as it prevents us from mixing up the number of goats with the number of cows
* performance is equivalent to using a simple Int
* newtype also has advantages related to typeclass instances
* can define typeclass instances that differ from instances for underlying type

```
class TooMany a where
    tooMany :: a -> Bool

instance TooMany Int where
    tooMany n = n > 42

-- can define custom instance for Goats instead
-- can't do that if it's a type synonym

instance TooMany Goats where
    tooMany (Goats n) = n > 43
```

* can also use `{-# LANGUAGE GeneralizedNewtypeDeriving #-}` to be able to reuse custom typeclass instances

```
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

class TooMany a where
    tooMany :: a -> Bool

instance TooMany Int where
    tooMany n = n > 42

newtype Goats = Goats Int deriving (Eq, Show, TooMany)
```

### Exercises: Logic Goats

1. see file

## Sum types

* `data Bool = False | True`
* two nullary constructors, which are each one
* cardinality of Bool is therefore 2

### Exercises: pity the bool

1. 4
2. 258 cardinality, overflow error for numbers bigger than 127 or smaller tahn -128

## Product types

* Cardinality is the product of cardinalities of the inhabitants
* Product is a way to carry multiple values in a dingle data constructor
* anonymous product: `( , ) :: a -> b -> (a, b)`

```
data QuantumBool = QuantumTrue
    | QuantumFalse
    | QuantumBoth
    deriving (Eq, Show)

data TwoQs =
    MkTwoQs QuantumBool QuantumBool
    deriving (Eq, Show)
```

* TwoQs has one data constructor taking two arguments 
* makes a product of the two types that inhabit it

```
MkTwoQs QuantumTrue QuantumTrue
MkTwoQs QuantumTrue QuantumFalse
MkTwoQs QuantumTrue QuantumBoth
MkTwoQs QuantumFalse QuantumFalse
```

* can also write TwoQs using a type alias and the tuple data constructor
* `type TwoQs = (QuantumBool, QuantumBool)`

### Record syntax

* records are product types with additional syntax
* provides convenient accessors to fields within record

```
data Person =
    MkPerson String Int
    deriving (Eq, Show)
```

* contains two type arguments, representing a name and an age
* has enormous cardinality

```
jm = MkPerson "julie" 108
ca = MkPerson "chris" 16

-- to get the name of a person
namae :: Person -> String
namae (MkPerson s _) = s
```

* now in record syntax:

```
data Person =
    Person { name :: String
        , age :: Int }
        deriving (Eq, Show)
```

* has named accessors for the fields
* are actually functions that go from a product type to a member of a product

```
:t name
name :: Person -> String

:t age
age :: Person -> Int

Person "Papu" 5
Person {name = "Papu", age = 5}

let papu = Person "Papu" 5
age papu == 5
name papu == "Papu"
```

## Normal form

* existing algebraic rules for products and sums apply in type systems
* includes distributive property

```
2 * (3 + 4)
2 * (7)
14

2 * 3 + 2 * 4
(6) + (8)
14
```

* can be generalized as `a * (b + c) -> (a * b) + (a * c)`
* now using types:

```
data Fiction = Fiction deriving Show
data Nonfiction = Nonfiction deriving Show

data BookType = FictionBook Fiction
        | NonfictionBook Nonfiction
        deriving Show
        
type AuthorName = String
data Author = Author (AuthorName, BookType)
```

* can apply the distributive property and rewrite Author in normal form
* cannot be simplified any further

```
data Author =
    Fiction AuthorName
    | Nonfiction AuthorName
    deriving (Eq, Show)
```

### Exercises: How does your garden grow?

1. .

```
data Garden =
    Gardenia Gardener
    | Daisy Gardener
    | Rose Gardener
    | Lilac Gardener
    deriving Show
```

## Constructing and deconstructing values

* two things to do with a value
* generate or construct it
* match on it and consume it

```
data Farmhouse =
    Farmhouse NumCow NumPig
    deriving (Eq, Show)

-- can be rewritten as

type Farmhouse' = Product NumCow NumPig

-- can also nest them

data BigFarmhouse =
    BigFarmhouse NumCow NumPig NumSheep
    deriving (Eq, Show)

type BigFarmhouse' =
    Product NumCow (Product NumPig NumSheep)

data Animal =
    Cow CowInfo
    | Pig PigInfo
    | Sheep SheepInfo
    deriving (Eq, Show)

type Animal' =
    Sum CowInfo (Sum PigInfo SheepInfo)
```

### Constructing values

```
trivialValue :: GuessWhat
trivialValue = Chickenbutt
```

* unary type constructor containing one unary data constructor
* `data Id a = MkId a deriving (Eq, Show)`
* have to apply it to something before constructing value of that type

```
idInt :: Id Integer
idInt = MkId 10
```

```
data Sum a b =
    First a
    | Second b
    deriving (Eq, Show)

data Twitter =
    Twitter deriving (Eq, Show)

data AskFm =
    AskFm deriving (Eq, Show)

socialNetwork :: Sum Twitter AskFm
socialNetwork = First Twitter
```

* type is a sum of Twitter or AskFm
* don't have both at the same time because sums express disjunction

```
myRecord :: RecordProduct Integer Float
myRecord = RecordProduct 42 0.00001

-- can take advantage of field to construct values differently
myRecord :: RecordProduct Integer Float
myRecord =
    RecordProduct { pfirst = 42
                , psecond = 0.00001 }
```

#### Exercise: programmers

1. `allProgrammers = [Programmer {os = x, lang = y} | x <- allOperatingSystems, y <- allLanguages]`


#### Accidental bottoms from records

* will get an error if you forget a field in a record
* either define the whole record at once or not at all
* can use partial application of data constructors instead

```
data ThereYet =
    There Float Int Bool
    deriving (Eq, Show)

notYet :: Int -> Bool -> ThereYet
notYet = nope 25.5

notQuite :: Bool -> ThereYet
notQuite = notYet 10

yusssss :: ThereYet
yusssss = notQuite False
```

### Deconstructing values

```
newtype Name = Name String deriving Show
newtype Acres = Acres Int deriving Show

data FarmerType = DairyFarmer
    | WheatFarmer
    | SoybeanFarmer
    deriving Show

data Farmer =
    Farmer Name Acres FarmerType
    deriving Show

isDairyFarmer :: Farmer -> Bool
isDairyFarmer (Farmer _ _ DairyFarmer) = True
isDairyFarmer _ = False

-- or using record syntax

data FarmerRec =
    FarmerRec { name :: Name
            , acres :: Acres
            , farmerType :: FarmerType }
            deriving Show

isDairyFarmerRec :: FarmerRec -> Bool
isDairyFarmerRec farmer =
    case farmerType farmer of
    DairyFarmer -> True
    _ -> False
```

## Function type is exponential

* given a function a -> b, inhabitants are b^a
* so Bool -> Bool would be 2^2
* `a -> b -> c` would be (c ^ b) ^ a
* or c ^ (b * a)

```
data Quantum =
    Yes
    | No
    | Both
    deriving (Eq, Show)

-- sum type: 3 + 3
quantSum1 :: Either Quantum Quantum
quantSum1 = Right Yes

-- product type: 3 * 3
quantProd1 :: (Quantum, Quantum)
quantProd1 = (Yes, Yes)

-- function type: 3 ^ 3
quantFlip1 :: Quantum -> Quantum
quantFlip1 Yes = Yes
quantFlip1 No = Yes
quantFlip1 Both = Yes
```

### Exercises: the quad

1. 8
2. 16
3. 256
4. 8
5. 16
6. 65536

## Higher-kinded datatypes

* `* -> *` is waiting for a single * before it is applied
* `* -> * ->*` must be applied twice
* that makes it a higher-kinded type
* Lists are higher-kinded types

```
data Silly a b c d =
    MkSilly a b c d deriving Show

:kind Silly
Silly :: * -> * -> * -> * -> *

:kind Silly Int
Silly Int :: * -> * -> * -> *

:kind Silly Int String
Silly Int String :: * -> * -> *

-- and so on

:kind (,,,)
(,,,) :: * -> * -> * -> * -> *

:kind (Int, String, Bool, String)
(Int, String, Bool, String) :: *
```

* kinds provide a generic way to express a hole to be filled by consumers of the datatype

```
data EsResultFound a =
    EsResultFound { _version :: DocVersion
                , _source :: a
    } deriving (Eq, Show)
```

* know that it will include a DocVersion value
* _source has type a because the structure of the documents being pulled are unknown
* will usually be a FromJSON typeclass instance
* but is not forced to be

## Lists are polymorphic

* lists are polymorphic because their type is not defined
* do not have an a until list's type's type argument has been fully applied

```
data [] a = [] | a : [a]
```

* when an operator has a non-alphanumeric name it's infix by default
* any operator starting with colon must be infix type or data constructor
* all infix data constructors must start with a colon
* list type without infix constructor:

`data List a = Nil | Cons a (List a)`

```
:kind List
List :: * -> *

:kind []
[] :: * -> *

:kind List Int
List Int :: *

:kind [Int]
[Int] :: *
```

## Binary Tree

* similar to lists
* can take an argument, and is recursive

```
data BinaryTree a =
    Leaf
    | Node (BinaryTree a) a (BinaryTree a)
    deriving (Eq, Ord, Show)
```

* has a value of type a at each node
* each node could be a leaf or could branch and have two subtrees
* subtrees also of type BinaryTree a
* can be more efficient for structuring data than a list
* but a tree that only branches right is indistinguishable from a list

### Inserting into trees

* need Ord to have enough information to arrange values
* insert function will insert value into tree or if no tree exists will give a means of building a tree
* values are immutable in haskell so when you insert a value into a data structure you build a new tree

```
insert' :: Ord a
            => a
            -> BinaryTree a
            -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
    | b == a = Node left a right
    | b < a = Node (insert' b left) a right
    | b > a = Node left a (insert' b right)
```

* base case handles the case of inserting an empty tree and beginning construction of new tree
* also handles case of reaching bottom of a larger tree

```
let t1 = insert' 0 Leaf
t1
Node Lead 0 Leaf

let t2 = insert' 3 t1
t2
Node Leaf 0 (Node Leaf 3 Leaf)

let t3 = insert' 5 t2
t3
Node Leaf 0
    (Node Leaf 3
        (Node Leaf 5 Leaf))
```

see file for exercises


## Chapter Exercises

### Multiple Choice

1. a
2. c
3. b
4. c

### Ciphers

see file

### As-patterns

see file