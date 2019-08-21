# Strings

## Types
* `:type __VAR__` in REPL to get type info
* `::` read as 'has the type'
* [] mean a list

## Printing
* `print` for general `putStr` and `putStrLn` for strings
* `"hello" ++ "world"` for concatenation
* or `concat [hello, " ", world]`
* `:: IO()` is type for output
* `functionName :: Integer -> Integer` example of type declaration for function

## Exercises
1. yes
2. no
3. no
4. yes

## More on type
* `:t (++)` returns `[a] -> [a] -> [a]`, takes lists and outputs list
* `:t concat` returns `[[a]] -> [a]`, takes list of lists and outputs list
* `Foldable t => t [a]` read as `[[a]]` for now
* a represents a particular type, means same type throughout function

## Exercises
1. `(++) [1, 2, 3] [4, 5, 6]`
2. `"<3" ++ "Haskell"`
3. No syntax error

## List functions
* String is list of chars so can use list functions on strings
* `(:)` called cons, builds list
* `'c' : "hris"` outputs `"chris"`
* `head` returns first element of list
* `head "Papuchon"` returns `'P'`
* tail returns everything except head
* `tail "Papuchon"` returns `"apuchon"`
* `take n` returns n number of elements
* `take 3 "Papuchon"` returns `"Pap"`
* `drop n` returns elements after n
* `drop 4 "Papuchon"` returns `"chon"`
* `(!!)` returns element at index
* `"Papuchon !! 4"` returns `'c'`
* these functions are unsafe because return error from empty list

## Chapter Exercises
### Syntax
1. 
   a. correct syntax
   b. incorrect syntax, should be `(++) [1, 2, 3] [4, 5, 6]`
   c. correct syntax
   d. incorrect syntax, should be `["hello" ++ " world"]`
   e. incorrect syntax, should be `"hello !! 4`
   f. correct syntax
   g. incorrect syntax, should be `take 4 "lovely"`
   h. correct syntax

2. a. returns d
   b. returns c
   c. returns e
   d. returns a
   e. returns b

### Functions
1. a. `"Curry is awesome" ++ "!"`
   b. `"Curry is awesome!" !! 4` 
    c. `drop 9 "Curry is awesome!"`
2. see file

## Definitions
1. A String is a sequence of characters. In Haskell, String is represented
by a linked-list of Char values, aka [Char].
2. A type or datatype is a classification of values or data. Types in
Haskell determine what values are members of the type or that
inhabit the type. Unlike in other languages, datatypes in Haskell
by default do not delimit the operations that can be performed
on that data.
3. Concatenation is the joining together of sequences of values.
Often in Haskell this is meant with respect to the [], or list,
datatype, which also applies to String which is [Char]. The concatenation
function in Haskell is (++) which has type [a] -> [a]
-> [a]. For example:
Prelude> "tacos" ++ " " ++ "rock"
"tacos rock"
4. Scope is where a variable referred to by name is valid. Another
word used with the same meaning is visibility, because if a variable
isn’t visible it’s not in scope.
5. Local bindings are bindings local to particular expressions. The
primary delineation here from top level bindings is that local
bindings cannot be imported by other programs or modules.
6. Top level bindings in Haskell are bindings that stand outside of
any other declaration. The main feature of top-level bindings
is that they can be made available to other modules within your
programs or to other people’s programs.
7. Data structures are a way of organizing data so that the data can
be accessed conveniently or efficiently.