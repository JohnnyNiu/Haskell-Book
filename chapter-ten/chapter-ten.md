# Folded Lists

* Folds are catamorphisms, which a means of constructing data
* If the spine is the structure of the list, a fold is what can reduce that structure

## Bringing you into the fold

* foldr, short for fold right is the fold used most often with lists
* `foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b` where t represents a list
* `foldr (+) 0 (1 : 2 : 3 : [])` = `1 + (2 + (3 + 0))`
* replaces the cons constructor with the function and reduces the list
* contrast with map which applies function to each member individually

## Recursive Patterns

* think of shape of recursive functions like sum, length, product, concat
* all have same structure, base case is identity for that function
* all have a main function with a recursive pattern that associates to the right, then evaluates the next head and so on

## Fold Right

one way to define foldr:

```
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f z [] = z
foldr f z (x:xs) = f x (foldr f z xs)
```

* very similar to the patterns mentioned above
* "rest of the fold" is `(foldr f z xs)` which is an argument to the function
* z is the zero case, a fallback value
* usually the identity for the function being folded

### How foldr evaluates

* another way to define it that is semantically equivalent to above:

```
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f z xs =
    case xs of
        [] -> z
        (x:xs) -> f x (foldr f z xs)
```

* this is how it evaluates when passed the sum function

```
foldr (+) 0 [1, 2, 3]

foldr (+) 0 [1, 2, 3] =
    case [1, 2, 3] of
        [] -> 0
        (x:xs) -> f x (foldr f z xs)

foldr (+) 0 [1, 2, 3] =
    case [1, 2, 3] of
        [] -> 0
        (1 : [2, 3]) -> (+) 1 (foldr (+) 0 [2, 3])

foldr (+) 0 [2, 3] =
    case [2, 3] of
        [] -> 0 -- this didn't match again
        (2 : [3]) -> (+) 2 (foldr (+) 0 [3])

foldr (+) 0 [3] =
    case [3] of
        [] -> 0 -- this didn't match again
        (3 : []) -> (+) 3 (foldr (+) 0 [])

foldr (+) 0 [] =
    case [] of
        [] -> 0 --<-- This one finally matches
```

* folding happens in two stages, traversal and folding
* traversal is when the fold recurses over the spine
* folding is the evaluation of the folding function applied to the values
* consequence is that if f doesn't evaluate its second argument, no more of the spine will be forced
* foldr can void evaluating some of the values in the list and also the list's spine
* can be used with lists that are potentially infinite
* first cons cell cannot be undefined, but evaluation as a whole is non-strict
* other cells can contain undefined if the evaluation doesn't reach them
* `foldr const 0 ([1,2] ++ undefined)` returns 1 because const just returns the first argument and throws away the second

## Fold Left

* due to the construction of lists, left folds traverse the spine in same direction as foldr
* folding process is left associative and goes the opposite direction to foldr

```
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f acc [] = acc
foldl f acc (x:xs) = foldl f (f acc x) xs

foldl (+) 0 (1 : 2 : 3 : [])

-- evaluates like this:
((0 + 1) + 2) + 3
```

* scans are similar to folds but return a list of each stage
* ` scanr (+) 0 [1..5]` [15,14,12,9,5,0]
* `scanl (+) 0 [1..5]` [0,1,3,6,10,15]
* `last (scanl f z xs) = foldl f z xs`
* `head (scanr f z xs) = foldr f z xs`
* result of both operations is the same but foldr and foldl arrive at them differently

### Associativity and folding

* fundamental way to think about evaluation in Haskell is as substitution
* when using a right fold, you're replacing the cons constructor with the folding function and the empty list constructor with the start value

```
[1..3] == 1 : 2 : 3 : []
foldr f z [1, 2, 3]
1 `f` (foldr f z [2, 3])
1 `f` (2 `f` (foldr f z [3]))
1 `f` (2 `f` (3 `f` (foldr f z [])))
1 `f` (2 `f` (3 `f` z))
```

* lazy evaluation lets functions, rather than semantics dictate the order things are evaluated
* means parentheses are important, decide the order things are evaluated or traversed

```
foldr (^) 2 [1..3]
(1 ^ (2 ^ (3 ^ 2)))
(1 ^ (2 ^ 9))
1 ^ 512
1

vs

foldl (^) 2 [1..3]
((2 ^ 1) ^ 2) ^ 3
(2 ^ 2) ^ 3
4 ^ 3
64

-- when applying cons to foldr, it evaluates to the input list
foldr (:) [] (1 : 2 : 3 : []) == 1 : (2 : (3 : []))


-- order of arguments are backwards for cons, so we have to use flip
foldl f z [1, 2, 3] == ((([] : 1) : 2) : 3)

f = flip (:)
((([] `f` 1) `f` 2) `f` 3)
 (([1] `f` 2) `f` 3)
  ([2, 1] `f` 3)
   [3, 2, 1]
```

### Exercises: Understanding Folds

1. `foldl (*) 1 [1..5]`
2. ,
```
foldl (flip (*)) 1 [1..3]
((1 * 2) * 3)
((2) * 3)
6
```
3. c
4. a
5. 1. `foldr (++) "woot" ["woot", "WOOT", "woot"]`
   1. `foldr max [] ["fear", "is", "the", "little", "death"]`
   2. `foldr (&&) True [False, True]`
   3. can't return a different answer
   4. `foldl (flip((++) . show)) "" [1..5]`
   5. `foldl const 'a' [1..5]` or `foldr const 0 [1..5]`
   6. `foldr (flip const) 0 "tacos"`
   7. `foldl const 0 "burritos"
   8. `foldl const 'z' [1..5]`

### Unconditional spine recursion

* difference between foldr and foldl is that foldl has steps of fold as first argument
* next recursion doesn't depend on the folding function
* still has to recurse the spine even if the function doesn't force evaluation of its elements
* `let xs = [1..5] ++ undefined`
* `foldl const 0 xs` has error, so does `foldl (flip const) 0 xs`
* foldl is generally inappropriate with infinite lists
* also inappropriate for long lists as forced evaluation of spine negatively effects performance
* usually if left fold is needed then should use `foldl'` which forces evaluation of values inside cons cells as it goes instead of accumulating unevaluated expressions for each element

## How to write fold functions

* think about what the start value of fold is, usually the identity value for the function
* then consider arguments, a and b
* a is one of the elements in the list, b is either the start value or value accumulated as list is processed
  
```
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr (\a b -> undefined) [] ["Pizza", "Apple", "Banana"]
-- replace [] with "" for semantic reasons
foldr (\a b -> undefined) "" ["Pizza", "Apple", "Banana"]
-- put in the function
foldr (\a b -> take 3 a) "" ["Pizza", "Apple", "Banana"]

foldr (\a b -> take 3 a) "" pab == "Piz"
foldl (\b a -> take 3 a) "" pab == "Ban"
```

* note difference in semantics for foldr and foldl
* need to use accumulation to get the actual result

```
let f = (\a b -> take 3 a ++ b)
foldr f "" pab == "PizAppBan"

let f' = (\b a -> take 3 a ++ b)
foldl f' "" pab == "BanAppPiz"
```

### Exercises: database processing

1. see file

## Folding and Evaluation

* right fold evaluates from the innermost cons cell to the outermost (the head)
* left fold recurses unconditionally to the end of the list through self-calls then the folding functione valuates from the outermost cons cell to the innermost
* foldl is equivalent to foldr if you flip the folding function and reverse the list

```
let xs = [1..5]
foldr (:) [] xs == [1,2,3,4,5]
foldl (flip (:)) [] xs == [5,4,3,2,1]
foldl (flip (:)) [] (reverse xs) == [1,2,3,4,5]
```

## Summary

### foldr
1. The rest of the fold (recursive invocation of foldr) is an argument
to the folding function you passed to foldr. It doesn’t
directly self-call as a tail-call like foldl. You could think of it
as alternating between applications of foldr and your folding
function 𝑓. The next invocation of foldr is conditional on 𝑓
having asked for more of the results of having folded the list.
That is:
foldr :: (a -> b -> b) -> b -> [a] -> b
-- ^
That 𝑏 we’re pointing at in (a -> b -> b) is the rest of the fold.
Evaluating that evaluates the next application of foldr.
2. Associates to the right.
3. Works with infinite lists. We know this because:
Prelude> foldr const 0 [1..]
1
4. Is a good default choice whenever you want to transform data
structures, be they finite or infinite.

### foldl

1. Self-calls (tail-call) through the list, only beginning to produce
values after reaching the end of the list.
2. Associates to the left.
3. Cannot be used with infinite lists. Try the infinite list example
earlier and your REPL will hang.
1. Is nearly useless and should almost always be replaced with
foldl' for reasons we’ll explain later when we talk about writing
efficient Haskell programs.

## Scans

* Scans accumulate values instead of keeping list's individual values separate, like folds
* Return a list of results, like maps
* List shows intermediate stages of evaluation
* Not used as often as folds and are very similar

```
scanr (+) 0 [1..3]
[1 + (2 + (3 + 0)), 2 + (3 + 0), 3 + 0, 0]
[6, 5, 3, 0]

scanl (+) 0 [1..3]
[0, 0 + 1,0 + 1 + 2, 0 + 1 + 2 + 3]
[0, 1, 3, 6]

scanl (+) 1 [1..3]
= [ 1, 1 + 1
    , (1 + 1) + 2
    , ((1 + 1) + 2) + 3
    ]
= [1, 2, 4, 7]
```

* the definition of scanl:

```
scanl :: (a -> b -> a) -> a -> [b] -> [a]
scanl f q ls =
    q : (case ls of
        [] -> []
        x:xs -> scanl f (f q x) xs)
```

* this is meant to return a list of fibonacci numbers but is a little broken:

```
fibs = 1 : scanl (+) 1 fibs

scanl (+) 1 [1..3]
[1, 1 + 1, (1 + 1) + 2, ((1 + 1) + 2) + 3]
[1,2,4,7]
```

### Getting the fibonacci number we want

* more useful to find the nth element or take some number of elements from the list
* (!!) will find the nth element

```
fibs = 1 : scanl (+) 1 fibs
fibsN x = fibs !! x
```

### Scans Exercises

1. `fibsTake20 = take 20  fibs`
2. `fibsUnder100 = takeWhile (\x -> x < 100) fibs`
3. `factorial = scanl (*) 1 [1..]`


## Chapter exercises

### Warum up and review

1.
   1. `[(a, b, c) | a <- "pdtdkg", b <- "aeiou", c <- "pdtdkg"]`
   2. `[(a, b, c) | a <- "p", b <- "aeiou", c <- "pdtdkg"]`

2. 1. find the average length of the words in the list
3. `seekritFunc x = (fromInteger(sum (map length (words x)))) / (fromInteger(length (words x)))`

### Rewriting functions using folds

1. `myOr = foldr (||) False` or `myOr = foldr (\a b -> a || b) False`
2. `myAny f = foldr (\x y -> f x || y ) False`
3. `myElem n xs = foldr (\x y -> n == x || y) False xs`
   1. `myElem n xs = any (\x -> z == x) xs`
4. `myReverse xs = foldl (flip (:)) [] xs`
5. `myMap = foldr ((:) .f) []`
6. `myFilter = foldr (\x y -> if f x then x : y else y) []`
7. `squish = foldr (\x y -> x ++ y) []`
8. `squishMap = foldr ((++) . f) []`
9. `squishAgain = squishMap id xs`
10. .
    ```
    myMaximumBy f (x:xs) = foldl fn x xs
        where fn a b =
            case (f a b) of
                GT -> a
                _ -> b
    ```
11. .
    ```
    myMinimumBy f (x:xs) = foldl fn x xs
        where fn a b =
            case (f a b) of
                LT -> a
                _ -> b
    ```

## Definitions

1. A fold is a higher-order function which, given a function to
accumulate the results and a recursive data structure, returns
the built up value. Usually a “start value” for the accumulation
is provided along with a function that can combine the type of
values in the data structure with the accumulation. The term
fold is typically used with reference to collections of values
referenced by a recursive datatype. For a generalization of
“breaking down structure”, see catamorphism.
2. A catamorphism is a generalization of folds to arbitrary datatypes.
Where a fold allows you to break down a list into an arbitrary
datatype, a catamorphism is a means of breaking down the
structure of any datatype. The bool :: a -> a -> Bool -> a function
in Data.Bool is an example of a simple catamorphism for a
simple, non-collection datatype. Similarly, maybe :: b -> (a ->
b) -> Maybe a -> b is the catamorphism for Maybe. See if you can
notice a pattern:
```
data Bool = False | True
bool :: a -> a -> Bool -> a
data Maybe a = Nothing | Just a
maybe :: b -> (a -> b) -> Maybe a -> b
data Either a b = Left a | Right b
either :: (a -> c)
-> (b -> c)
-> Either a b
-> c
```
3. A tail call is the final result of a function. Some examples of tail
calls in Haskell functions:
```
f x y z = h (subFunction x y z)
where subFunction x y z = g x y z
-- the ``tail call'' is
-- h (subFunction x y z)
-- or more precisely, h.
```
4. Tail recursion is a function whose tail calls are recursive invocations
of itself. This is distinguished from functions that call
other functions in their tail call.
`f x y z = h (subFunction x y z)`
`where subFunction x y z = g x y z`
The above is not tail recursive, calls ℎ, not itself.
`f x y z = h (f (x - 1) y z)`
Still not tail recursive. 𝑓 is invoked again but not in the tail call
of 𝑓; it’s an argument to the tail call, ℎ:
`f x y z = f (x - 1) y z`
This is tail recursive. 𝑓 is calling itself directly with no intermediaries
```
foldr f z [] = z
foldr f z (x:xs) = f x (foldr f z xs)
```
Not tail recursive, we give up control to the combining function
𝑓 before continuing through the list. foldr’s recursive calls will
bounce between foldr and 𝑓.
```
foldl f z [] = z
foldl f z (x:xs) = foldl f (f z x) xs
```
Tail recursive. foldl invokes itself recursively. The combining
function is only an argument to the recursive fold.