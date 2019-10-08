# Signalling Adversity

* Doesn't always make sense for a program to have an output for every member of a datatype
* Need some way to signal that inputs don't make sense

## How I learned to stop worrying and love Nothing

* `data Maybe a = Nothing | Just a`
* example where there is no output for odd numbers:

```
ifEvenAdd2 :: Integer -> Integer
ifEvenAdd2 n = if even n then n+2 else ???

-- can use nothing to signal lack of output

ifEvenAdd2 :: Integer -> Maybe Integer
ifEvenAdd2 n = if even n then n+2 else Nothing

-- but also need to use Just for the actual output

ifEvenAdd2 :: Integer -> Maybe Integer
ifEvenAdd2 n = if even n then Just (n+2) else Nothing
```

* parentheses are necessary because function binding has higher precedence than +

### Smart constructors for datatypes

* Person is a product type made up of a name and an age

```
type Name = String
type Age = Integer
data Person = Person Name Age deriving Show
```

* we could still define a person with an empty string for a name or a negative age so we can be more accurate:

```
mkPerson :: Name -> Age -> Maybe Person
mkPerson name age
    | name /= "" && age >= 0 = Just $ Person name age
    | otherwise = Nothing
```

* this is called a smart constructor
* only allows constructing a type when the values make sense for that type

## Bleating either

* What if we want a way to signal which part of the constructor went awry
* For this we use the Either datatype
* `data Either a b = Left a | Right b`
* Can create a sum datatype to signal which part was incorrect
* `data PersonInvalid = NameEmpty | AgeTooLow deriving (Eq, Show)`
* Will compile without Eq but is necessary to use == in guard expressions
* Putting this all together:

```
type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid = NameEmpty | AgeTooLow deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
    | name /= "" && age >= 0 = Right $ Person name age
    | name == "" = Left NameEmpty
    | otherwise = Left AgeTooLow
```

* Now the type expression for mkPerson returns an either result
* Left result is PersonInvalid, Right result is a valid Person
* First case is equivalent to the original mkPerson
* Other cases return a PersonInvalid with a value signalling what went wrong
* Conventional to use Left to signal an error but Left is also used for this because it will stop the program. Certain expressions will not map over the leftmost function
* This isn't perfect because it doesn't express a list of errors

```
type Name = String
type Age = Integer
type ValidatePerson a = Either [PersonInvalid] a

data Person = Person Name Age deriving Show
data PersonInvalid = NameEmpty | AgeTooLow deriving (Eq, Show)
```

* Only difference is that ValidatePerson abstracts the Either expression
* Checking functions for our type:

```
ageOkay :: Age -> Either [PersonInvalid] Age
ageOkay age = case age >= 0 of
    True -> Right age
    False -> Left [AgeTooLow]

nameOkay :: Name -> Either [PersonInvalid] Name
nameOkay name = case name /= "" of
    True -> Right name
    False -> Left [NameEmpty]
```

* Now using this to validate our Person inputs:

```
mkPerson :: Name -> Age -> ValidatePerson Person
mkPerson name age = mkPerson' (nameOkay name) (ageOkay age)

mkPerson' :: ValidatePerson Name -> ValidatePerson Age -> ValidatePerson Person
mkPerson' (Right nameOk) (Right ageOk) = Right (Person nameOk ageOk)
mkPerson' (Left badName) (Left badAge) = Left (badName ++ badAge)
mkPerson' (Left badName) _ = Left badName
mkPerson' _ (Left badAge) = Left badAge
```

* Uses a helper function that allows us to pattern match based on the outputs of our checking functions
* Either returns a Person or a list of errors
* ValidatePerson can take any type as its second argument and in this case takes all of Name, Age and Person
* This can be abstracted away into a slimmer function but this won't be covered in this chapter

```
mkPerson :: Name-> Age -> Validation [PersonInvalid] Person
mkPerson name age = liftA2 Person (nameOkay name) (ageOkay age)
```

## Kinds, a thousand stars in your types

* Kinds are used to describe the types of type constructors
* Haskell is notable for higher-kinded types
* This means that types can take more types as arguments

```
data Example a = Blah | RoofGoats | Woot a
* -> *
-- needs one argument so has that kind signature

:k (,)
* -> * -> *
-- two-tuple has two arguments that can be of two different types

:k Maybe
Maybe :: * -> *
:k Maybe Int
Maybe Int :: *
-- Maybe takes one type in its constructor, once it is constructed, it goes back to a single *, a concrete type

:k Either
Either :: * -> * -> *
-- Either needs two type arguments to construct
```

### Lifted and unlifted types

* \* is the kind of all lifted types
* unlifted types have kind #
* Lifted is defined as any type that can be inhabited by bottom
* They are represented by a pointer
* Types of kind # are often native machine types and raw pointer
* Newtypes are kind * but they don't create a new pointer, an exception
* Newtype cannot itself be inhabited by bottom

```
:k Maybe Maybe
-- does not work, because Maybe expects an argument with kind *
-- Maybe itself has kind * -> * so is not a valid argument

:k Maybe (Maybe Char)
-- this works because once Char is applied to Maybe, it becomes type *
```

* This is not immediately important other than understand compiler errors but will be clear in a later chapter

### Data constructors are functions

* Once a data constructor is applied to an argument it returns a value of the appropriate type
* This means they are functions
* Can be curried like functions
* Example using fmap to map Just to a list:

```
fmap Just [1, 2, 3] = 
[Just 1,Just 2,Just 3]
```

* Utility may not be clear until later chapters

## Chapter Exercises

### Determine the kinds

1. * -> *
2. * -> * -> *

### String processing

1. see file