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



