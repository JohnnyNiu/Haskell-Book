# Recursion

* Defining a functions in terms of itself via self-referential expressions
* Call itself and reapeat behaviour until some condition is met
* Uses something called a Y combinator to achieve this

## Factorial

* `brokenFact1 :: Integer -> Integer; brokenFact1 n = brokenFact1 (n - 1)`
* Doesn't work because it has no condition to stop it

```
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)
```

* sets a base case of 0 = 1 so once it reaches 0 it just returns 1

### Another way to look at recursion

* Can think of recursion in terms of function composition
* Difference is recursion keeps applying until base case is met

```
incTimes :: (Eq a, Num a) => a -> a -> a
incTimes 0 n = n
incTimes times n = 1 + (incTimes (times - 1) n)
```

* will apply the number of times entered as the times variable
* will terminate once it gets 0
* or more generally, taking any function and showing function composition:

```
applyTimes :: (Eq a, Num a) => a -> (b -> b) -> b -> b
applyTimes 0 f b = b
applyTimes n f b = f . applyTimes (n-1) f $ b
```

### Intermission: exercise

1. applyTimes 5 (+1) 5
2. applyTimes 4 (+1) 6
3. applyTimes 3 (+1) 7
4. applyTimes 2 (+1) 8
5. applyTimes 1 (+1) 9
6. applyTimes 0 (+1) 10
7. 10

## Bottom

* ⊥ or bottom used to refer to functions that don't successfully return a value
* symbol corresponds to false in logic
* `let x = x in x` returns an error because it's a never-ending computation

``` 
f :: Bool -> Int
f False = 0
```

* incomplete function that doesn't handle all cases, inputting f True is an example of bottom
* above partial function really translates to

```
f :: Bool -> Int
f False = 0
f _ = error $ "*** Exception: "
            ++ "Non-exhaustive"
            ++ "patterns in function f"
```

## Fibonacci Numbers

* another classic example of recursion
* Fibonacci numbers always positive integers so type signature will be:
* `fibonacci :: Integral a => a -> a`
* or `fibonacci :: Integer -> Integer`
* needs two base cases, `fibonacci 0 = 0` and `fibonacci 1 = 1`
* needs an argument that can be repeatedly passed to itself
* also need code to get the two numbers before the argument
* `fibonacci x = (x - 1) (x - 2)`
* this won't work how it is, we need recursion
* also, we want the fibonacci sequence to do as many steps as our argument
* `fibonacci x = fibonacci (x - 1) + fibonacci (x - 2)`

## Integral division from scratch

* can define integral division in terms of substraction
* type will be `dividedBy :: Integer -> Integer -> Integer`
* base case could be 0 but not all numbers divide evenly
* in this case, returns tuple with the quotient and remainder

```
dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
    where go n d count
        | n < d = (count, n)
        | otherwise = go (n - d) d (count + 1)
```

* go allows where clause to have more arguments than the top level function
* in this case introduces count to keep track of how many iterations we've done
* n and d are just names for num and denom
  
```
dividedBy 10 2

10 divided by 2 ==
10  - 2, 8 (subtracted 1 time)
    - 2, 6 (subtracted 2 times)
    - 2, 4 (subtracted 3 times)
    - 2, 2 (subtracted 4 times)
    - 2, 0 (subtracted 5 times)
```

```
dividedBy 10 2 =
go 10 2 0

    | 10 < 2 = ...
    -- false, skip this branch
    
    | otherwise = go (10 - 2) 2 (0 + 1)
    -- otherwise is literally the value True
    -- so if first branch fails,
    -- this always succeeds
    
    go 8 2 1
    -- 8 isn't < 2, so the otherwise branch
    go (8 - 2) 2 (1 + 1)
    -- n == 6, d == 2, count == 2

    go 6 2 2
    go (6 - 2) 2 (2 + 1)
    -- 6 isn't < 2, so the otherwise branch
    -- n == 4, d == 2, count == 3

    go 4 2 3
    go (4 - 2) 2 (3 + 1)
    -- 4 isn't < 2, so the otherwise branch
    -- n == 2, d == 2, count == 4
    
    go 2 2 4
    go (2 - 2) 2 (4 + 1)
    -- 2 isn't < 2, so the otherwise branch
    -- n == 0, d == 2, count == 5
    
    go 0 2 5
    -- the n < d branch is finally evaluated
    -- because 0 < 2 is true
    -- n == 0, d == 2, count == 5
    | 0 < 2 = (5, 0)

(5, 0)
```

## Chapter Exercises

### Review of types

1. `[[Bool]]`
2. `[[3 == 3], [6 > 5], [3 < 4]]`
3. all of the above
4. `func "Hello" "World!"`

### Reviewing Currying

1. "woops mrow woohoo!"
2. "1 mrow haha"
3. "woops mrow 2 mrow haha"
4. "woops mrow blue mrow haha" 
5. "pink mrow haha green mrow woops mrow blue"
6. "are mrow Pugs mrow awesome"

### Recursion

1. .

```
dividedBy 15 2
    go 15 2 0
    otherwise = go (15 - 2) 2 (0 + 1)

    go 13 2 1
    go (13 - 2) 2 (1 + 1)

    go 11 2 2
    go (11 - 2) 2 (2 + 1)

    go 9 2 3
    go (9 - 2) 2 (3 + 1)

    go 7 2 4
    go (7 - 2) 2 (4 + 1)

    go 5 2 5
    go (5 - 2) 2 (5 + 1)

    go 3 2 6
    go (3 - 2) 2 (6 + 1)

    go 1 2 7
    1 < 2 = (7, 1)
```

2. see file