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
* 