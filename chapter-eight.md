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