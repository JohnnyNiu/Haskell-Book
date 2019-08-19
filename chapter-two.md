# Chapter Two

## GHCi
* `ghci` or `stack ghci` to start environment
* `:quit` to exit
* `:load test.hs` to run file called test.hs then call functions from file
* `:m` or `:module` to exit file

## Expressions
* Anything that evaluates to a result in Haskell
* Includes arithmetic, functions, literals (which evaluate to themselves)
* Normal form = no more steps needed to evaluate expression
* Reducible expressions are redexes
  
## Functions
* Functions take shape of name then parameters then expression
* i.e. `triple x = x * 3`
* parameters correspond to head of lambda
* right of the declaration corresponds to body of lambda
* in GHCi `let triple x = x * 3`
* in file `triple x = x * 3`

## Evaluation
* Haskell uses lazy evaluation, only evaluates things when needed
* Adding parameter to function causes evaluation
* e.g. 1 + 1 evaluates to 2
* `triple 3` evaluates to 9

## Comprehension Check
2. pi x = x * 3.14
3. pi x = x * pi

## Infix
* e.g. arithmetic operation 2 + 2
* can also be written as (*) 2 2
* reverse is div 10 4 to 10 \`div\` 4

## Associativity and Precedence
* Use :info to find out about operator or function
* Tells whether left or right associative and precedence using a number

## Exercises
1. 8 + 7 * 9 parantheses make a difference
2. `perimeter x y = x * 2 + y * 2` parentheses don't matter
3. `f x = x / 2 + 9` parentheses matter

## Modules
* new file learn.hs
* `module Learn where` at top of file
* `:l learn.hs` to load

## Exercises
1. `let area x = 3.14 * (x * x)`
2. `let double x = x * 2`
3. whitespace issue
```
x = 7 
y = 10
f = x + y
```

## Arithmetic Operations
Operator        Name       Purpose/application
+               plus        addition
-               minus       subtraction
*               asterisk    multiplication
/               slash       fractional division
div             divide      integral division, round down
mod             modulo      like ‘rem’, but after modular division
quot            quotient    integral division, round towards zero
rem             remainder   remainder after division

## Negatives
* Sometimes need to use brackets for negative numbers to avoid confusion with infixes

## $
* Lowest precedence
* Used to refer to things with less parentheses
* (2^) (2 + 2) becomes (2^) $ 2 + 2
  
## Sectioning
* Can do (+1) 2 instead of 1 + 2 or (+) 1 2

## Where
* 
```
printInc n = print plusTwo
  where plusTwo = n + 2 
```
* alternatively
```
printInc2 n = let plusTwo = n + 2
              in print plusTwo
```

## Exercises
1 `let x = 5 in x` 5
2. `let x = 5 in x * x` 25
3. `let x = 5; y = 6 in x * y` 30
4. `let x = 3; y = 1000 in x + 3` 6


## Chapter Exercises
### Parenthisization
1. 2 + (2 * 3) - 1
2. (^) 10 (1 + 1)
3. (2 ^ 2) * (4 ^ 5) + 1

### Equivalent Expressions
1. Yes
2. Yes
3. No
4. No
5. No

## Definitions
1. The terms argument and parameter are often used interchangeably.
However, it is worthwhile to understand the distinction.
A parameter, or formal parameter, represents a value that will
be passed to the function when the function is called. Thus,
parameters are usually variables. An argument is an input value
the function is applied to. A function’s parameter is bound to
the value of an argument when the function is applied to that
argument. For example, in f x = x + 2 which takes an argument
and returns that value added to 2, 𝑥 is the one parameter of our
function. We run the code by applying 𝑓 to some argument. If
the argument we passed to the parameter 𝑥 were 2, our result
would be 4. However, arguments can themselves be variables
or be expressions that include variables, thus the distinction
is not always clear. When we use “parameter” in this book,
it will always be referring to formal parameters, usually in a
type signature, but we’ve taken the liberty of using “argument”
somewhat more loosely.
2. An expression is a combination of symbols that conforms to syntactic
rules and can be evaluated to some result. In Haskell, an
expression is a well-structured combination of constants, variables,
and functions. While irreducible constants are technically
expressions, we usually refer to those as “values”, so we usually
mean “reducible expression” when we use the term expression.
1. A value is an expression that cannot be reduced or evaluated any
further. 2 * 2 is an expression, but not a value, whereas what it
evaluates to, 4, is a value.
4. A function is a mathematical object whose capabilities are limited
to being applied to an argument and returning a result.
Functions can be described as a list of ordered pairs of their
inputs and the resulting outputs, like a mapping. Given the
function f x = x + 2 applied to the argument 2, we would have
the ordered pair (2, 4) of its input and output.
5. Infix notation is the style used in arithmetic and logic. Infix
means that the operator is placed between the operands or
arguments. An example would be the plus sign in an expression
like 2 + 2.
6. Operators are functions that are infix by default. In Haskell,
operators must use symbols and not alphanumeric characters.
7. Syntactic sugar is syntax within a programming language designed
to make expressions easier to write or read.