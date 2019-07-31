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
### Combinators
1. 𝜆𝑥.𝑥𝑥𝑥 Yes
2. 𝜆𝑥𝑦.𝑧𝑥 No
3. 𝜆𝑥𝑦𝑧.𝑥𝑦(𝑧𝑥) Yes
4. 𝜆𝑥𝑦𝑧.𝑥𝑦(𝑧𝑥𝑦) Yes
5. 𝜆𝑥𝑦.𝑥𝑦(𝑧𝑥𝑦) No

### Normal form or diverge?
1. 𝜆𝑥.𝑥𝑥𝑥 Normal form
2. (𝜆𝑧.𝑧𝑧)(𝜆𝑦.𝑦𝑦) Diverge
3. (𝜆𝑥.𝑥𝑥𝑥)𝑧 Normal form

### Beta reduce
1. (𝜆𝑎𝑏𝑐.𝑐𝑏𝑎)𝑧𝑧(𝜆𝑤𝑣.𝑤)
a. (𝜆𝑤𝑣.𝑤)𝑧𝑧
b. 𝑧

2. (𝜆𝑥.𝜆𝑦.𝑥𝑦𝑦)(𝜆𝑎.𝑎)𝑏
a. (𝜆𝑦.(𝜆𝑎.𝑎)𝑦𝑦)𝑏
b. (𝜆𝑎.𝑎)bb
c. bb

3. (𝜆𝑦.𝑦)(𝜆𝑥.𝑥𝑥)(𝜆𝑧.𝑧𝑞)
a. (𝜆𝑥.𝑥𝑥)(𝜆𝑧.𝑧𝑞)
b. (𝜆𝑧.𝑧𝑞)(𝜆𝑧.𝑧𝑞)
c. (𝜆𝑧.𝑧𝑞)𝑞
d. 𝑞𝑞

4. (𝜆𝑧.𝑧)(𝜆𝑧.𝑧𝑧)(𝜆𝑧.𝑧𝑦)
a. (𝜆𝑧.𝑧)(𝜆m.mm)(𝜆n.n𝑦)
b. (𝜆m.mm)(𝜆n.n𝑦)
c. (𝜆n.n𝑦)(𝜆n.n𝑦)
d. (𝜆n.n𝑦)𝑦
e. 𝑦𝑦

5. (𝜆𝑥.𝜆𝑦.𝑥𝑦𝑦)(𝜆𝑦.𝑦)𝑦
a. (𝜆𝑥.𝜆𝑦.𝑥𝑦𝑦)(𝜆m.m)n
b. (𝜆𝑦.(𝜆m.m)𝑦𝑦)n
c. (𝜆m.m)nn
d. nn (aka yy)

6. (𝜆𝑎.𝑎𝑎)(𝜆𝑏.𝑏𝑎)𝑐
a. (𝜆𝑎.𝑎𝑎)(𝜆𝑏.𝑏𝑎)𝑐
b. (𝜆𝑏.𝑏𝑎)(𝜆𝑏.𝑏𝑎)𝑐
c. (𝜆𝑏.𝑏𝑎)ac
d. aac

7. (𝜆𝑥𝑦𝑧.𝑥𝑧(𝑦𝑧))(𝜆𝑥.𝑧)(𝜆𝑥.𝑎)
a. (𝜆𝑥.𝑧)𝑧((𝜆𝑥.𝑎)𝑧)
b. 𝑧((𝜆𝑥.𝑎)𝑧)
c. 𝑧𝑎
^Not quite right. Distinguish between the two z's 
with alpha equivalence

## Definitions

1. The lambda in lambda calculus is the greek letter 𝜆 used to introduce, or abstract, arguments for binding in an expression.
2. A lambda abstraction is an anonymous function or lambda term.
(𝜆𝑥.𝑥 + 1)
The head of the expression, 𝜆𝑥., abstracts out the term 𝑥 + 1. We can apply it to any 𝑥 and recompute different results for each 𝑥 we applied the lambda to.
3. Application is how one evaluates or reduces lambdas, this binds the argument to whatever the lambda was applied to. Computations are performed in lambda calculus by applying lambdas to arguments until you run out of arguments to apply lambdas
to.
(𝜆𝑥.𝑥)1
This example reduces to 1, the identity 𝜆𝑥.𝑥 was applied to the value 1, 𝑥 was bound to 1, and the lambda’s body is 𝑥, so it just kicks the 1 out. In a sense, applying the 𝜆𝑥.𝑥 consumed it. We reduced the amount of structure we had.
4. Lambda calculus is a formal system for expressing programs in terms of abstraction and application.
5. Normal order is a common evaluation strategy in lambda calculi. Normal order means evaluating (ie, applying or beta reducing) the leftmost outermost lambdas first, evaluating terms nested
within after you’ve run out of arguments to apply. Normal order isn’t how Haskell code is evaluated - it’s call-by-need instead.
We’ll explain this more later. Answers to the evaluation exercises were written in normal order.