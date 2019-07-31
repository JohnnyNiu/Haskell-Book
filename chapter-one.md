# Chapter One

## Key Concepts 
### Lambda Expression
* Head and body
* Head contains bound variable
### Alpha equivalence
* Letters used don't change meaning except for free variables
### Beta reducation
* Use value for bound variable to reduce expression
### Free variables
* Variables not bound to a head
### Currying
* Abstractions with multiple variables are really multiple abstractions
* Reduce from left to right where possible
### Normal form
* Cannot be reduced
### Combinators
* No free variables
### Divergence
* When lambdas don't reduce (or converge) to normal form
* Doesn't produce meaningful result

## Intermission - alpha equivalence

1. 𝜆𝑥𝑦.𝑥𝑧
b) 𝜆𝑚𝑛.𝑚𝑧

2. 𝜆𝑥𝑦.𝑥𝑥𝑦
c) 𝜆𝑎.(𝜆𝑏.𝑎𝑎𝑏)

3. 𝜆𝑥𝑦𝑧.𝑧𝑥
b) 𝜆𝑡𝑜𝑠.𝑠𝑡

## Summary
* Functional programming is based on expressions that include variables or constant values, expressions combined with other expressions, and functions.
* Functions have a head and a body and are those expressions that can be applied to arguments and reduced, or evaluated, to a result.
* Variables may be bound in the function declaration, and every time a bound variable shows up in a function, it has the same value.
* All functions take one argument and return one result.
* Functions are a mapping of a set of inputs to a set of outputs. Given the same input, they always return the same result.

## Exercises
