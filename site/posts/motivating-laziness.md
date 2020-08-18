---
title: "Intro to FP Through λ-Calc Part 1. - Motivating Laziness"
author: Jonathan Lorimer
date: 19/03/2020
datePretty: Mar 19, 2020
description: "Some of the core, lambda calculus specific, learnings I got from reading An Introduction to Functional Programming Through Lambda Calculus."
tags: [lambda-calculus, book-review]
---

# Table of Contents

- [Introduction](#introduction)
- [Terminology & Termination](#terminology-&-termination)
- [A Lazy Solution](#a-lazy-solution)

# Introduction

I am planning on writing a series of blog posts, of which this is the first, on _An Introduction to Functional Programming Through Lambda Calculus_ (referred to as the book throughout, even an acronym was obnoxious) by Greg Michaelson. Prior to reading this book I already had a cursory understanding of the lambda calculus, but I wanted to solidify my understanding as well as find connections to my primary area of interest; Haskell. The format of these posts won't be a review of the book, but rather an exposition of the three most interesting observations for me. This particular article is going to look at evaluation order. Prior to reading the book I found evaluation order an impenetrable subject, it reminded me of order of operations from grade school math, and most explanations I came across were crucially missing the _why_. I suppose evaluation order is a rather low-level, procedural mechanism, and is not often paired with theory? The tutoring I received did give me enough of an understanding of evaluation order to navigate Haskell's laziness. However, I found the theory quite helpful in this case.

> N.B. this post assumes basic familiarity with the syntax of lambda calculus [this is not a bad starting point](https://personal.utdallas.edu/~gupta/courses/apl/lambda.pdf)

#### Attribution

The majority of the content that I reference from the book occurs between pages 187 and 205 (CH8. Evaluation). I will explicitly cite any direct quotations, or references that occur outside of these pages, but pretty much all of the contents of this blogpost come from Michaelson so to avoid tedious citation I am directing you to Chapter 8 now.

# Terminology & Termination

#### Normal Form

**Normal form** is the description of a λ-expression which cannot be β-reduced any further. This means that all external and internal function applications are reduced
```
1. (λx.y.z.xy) (λa.a) (λb.c.b)  =>
2. (λy.z.(λa.a) y) (λb.c.b)     =>
3. λz.(λa.a)(λb.c.b)            =>
4. λz.b.c.b
```
lines 1-3 represent the reduction of external function applications, while line 4 represents an internal function application (because it occurs inside the function body). The reduction of internal applications is important because some expressions that would reduce to the same normal form appear different when internal applications are not reduce. For example, in the code block below, at line 2 the λ-expression looks different than the λ-expression at line 3 from before, however it is clear at end of reduction both λ-expressions are the same.
```
1. (λy.z.b.yb) (λx.c.x) =>
2. λz.b.(λx.c.x)b       =>
3. λz.b.c.b
```

This brings about an important point, by the Church-Rosser theorem, all normal forms are unique. The Church-Rosser theorem says that if λ-expression `A` can be λ-converted (or λ-abstracted) to `B` (i.e. replace a concrete value with a bound variable through λ-abstraction and then an apply that function to the concrete value) then both `A` and `B` reduce to normal form `C` (Turner 2004:189). Below is an example of λ-conversion.
```
λa.x.x(λb.b)     => λ-abstraction
(λc.a.x.xc)(λb.b)
```
A term is said to be **Normalizing** when it has not yet been reduced to normal form. Normal form is important in lambda calculus because it is effectively how computation is carried out, the reduction of a normalizing term shares a parallel with elimination rules in formal logic, or the evaluation of an arithmetic phrase.


#### Intermediate Normal Forms

There are two intermediate normal forms. The first is **Weak Head Normal Form** (WHNF), this is a function whose body contains a function or functions that can be applied further (an unevaluated function application is formally referred to as a **Redex**). Line 1 below is _NOT_ in WHNF because the outermost λ-expression is itself a **Redex**, however once the last external function application has been applied (line 3) the expression is in WHNF. The next reduction (line 4) happens "under the lambda", moving us past WHNF, but we still have valid reduction moves left. It is only once we reach line 5 that we have reached our second intermediate normal form; **Head Normal Form** (HNF). The definition of HNF is that there are no external _OR_ internal redexes, but the λ-expression is not fully evaluated because there is an unsubstituted bound variable that is preventing evaluation. However, if we apply our λ-expression to one more term (after the dashed line) we see that we can continue reduction until we reach **Normal Form** (line 8).
```
1. (λv.x.y.(xy)(xv))(λa.a)(λz.z) =>
2. (λv.x.y.(xy)(x(λa.a)))(λz.z)  =>
3. λy.((λz.z)y)((λz.z)(λa.a))    =>
4. λy.y((λz.z)λa.a)              =>
5. λy.y(λa.a)                    =>
-----------------------------------
6. (λy.y(λa.a))(λx.x)
7. (λx.x)(λa.a)
8. λa.a
```
#### Reduction Orders

**Normal Order** (NO) reduction evaluates the leftmost redex by passing the argument _unevaluated_, while **Applicative Order** (AO) evaluates that argument that is being passing. NO reduction has the benefit of terminating more frequently than AO evaluation, but at the cost of a potentially more expensive reduction process. To clarify, all λ-expressions that terminate upon AO reduction will also terminate upon NO reduction, but not necessarily the other way around. Below are two examples of the differences between AO and NO reduction, the first highlights the inefficiency of NO, while the second illustrates non-termination of AO.

```
-- Normal Order Reduction
1. (λx.xx)((λa.b.c.c)(λs.s)(λt.t))                  =>
2. ((λa.b.c.c)(λs.s)(λt.t))((λa.b.c.c)(λs.s)(λt.t)) =>
3. (λb.c.c)(λt.t))((λa.b.c.c)(λs.s)(λt.t))          =>
4. (λc.c)((λa.b.c.c)(λs.s)(λt.t))                   =>
5. (λc.c)((λb.c.c)(λt.t))                           =>
6. (λc.c)(λc.c)                                     =>
7. λc.c

-- Applicative Order Reduction
1. (λx.xx)((λa.b.c.c)(λs.s)(λt.t)) =>
2. (λx.xx)((λb.c.c)(λt.t))         =>
3. (λx.xx)(λc.c)                   =>
4. (λc.c)(λc.c)                    =>
5. (λc.c)
```

```
-- Normal Order Reduction
1. (λf.(λs.f(s s))(λs.f(s s)))(λx.y.y)             =>
2. (λs.(λx.y.y)(s s))(λs.(λx.y.y)(s s)))           =>
3. (λx.y.y)((λs.(λx.y.y)(s s))(λs.(λx.y.y)(s s)))  =>
4. (λy.y)                                          =>

-- Applicative Order Reduction
1. (λf.(λs.f(s s))(λs.f(s s)))(λx.y.y)             =>
2. (λs.(λx.y.y)(s s)) (λs.(λx.y.y)(s s))           =>
3. (λx.y.y)
    ((λs.(λx.y.y)(s s)) (λs.(λx.y.y)(s s)))        =>
4. (λx.y.y)
    ((λx.y.y)
      ((λs.(λx.y.y)(s s)) (λs.(λx.y.y)(s s))))     =>
5. (λx.y.y)
    ((λx.y.y)
      ((λx.y.y)
        ((λs.(λx.y.y)(s s)) (λs.(λx.y.y)(s s)))))  =>
6. (λx.y.y)
    ((λx.y.y)
      ...
        ((λs.(λx.y.y)(s s)) (λs.(λx.y.y)(s s)))))  =>
```

The non-terminating example is a little bit complex, so let's dig in. There are two parts `(λf.(λs.f(s s))(λs.f(s s)))` and `(λx.y.y)`. The first λ-expression is a common combinator known as a fixpoint, it is a higher order function, that represents a general approach to recursion in the simply typed lambda calculus (basic self application `(λs.ss)λs.ss` doesn't typecheck). It is not important to understand the fixpoint combinator now, but what is important is to recognize that this kind of recursion is impossible in applicative order evaluation, because the generative self application is evaluated at every invocation, and therefore there is no opprtunity for the logic to fork to a base case. The second λ-expression was chosen trivially, because it terminates instantly; I didn't want to have to think too hard about a recursive example that terminates in normal order evaluation. However, this function could have been any function at all, and the applicative order evaluation would never terminate.

Another important feature of delayed NO evaluation is that it allows for _codata_, otherwise known as infinite data structures. In the next section we will look at how one can achieve the benefits of both evaluation strategies, but first a summary of the terms we have looked at.

#### Section Summary

**Normal Form**: A fully evaluated λ-expression
**Head Normal Form**: A λ-expression that has all external and internal redexes evaluated, but is not fully evaluted. The only way to achieve a situation like this is to have a bound variable as the leftmost function expression in the body of a λ-expression, whose substitution is delaying evaluation of the expression i.e. `λy.(y(λx.x))(λz.z)`
**Weak Head Normal Form**: A λ-expression that has been evaluated up to the last λ-abstraction, but the body is still unevaluated.
**Redex**: An unevaluated function application
**Normal Order**: Evaluation by substituting the unevaluated λ-expression for all the bound variables that share the same name as the argument
**Applicative Order**: Evaluation by evaluated λ-expression that the function is applied to, and then substituting the normal form for all the bound variables that share the same name as the argument

For those of you who are familiar with Haskell, [monoidmusician](https://github.com/monoidmusician) wrote out a tiny DSL and some predicates to help understand the definitions:

```haskell
data LC = Abs LC | App LC LC | Var Int

isRedex :: LC -> Bool
isRedex (App (Abs f) a) = True
isRedex _ = False

isNF :: LC -> Bool
isNF t | isRedex t = False
isNF (Abs t) = isNF t
isNF (App f a) = isNF f && isNF a
isNF _ = True

isHNF :: LC -> Bool
isHNF t | isRedex t = False
isHNF (Abs t) = isHNF t -- this is the difference between WHNF and HNF!
isHNF (App f a) = isHNF f
isHNF _ = True

isWHNF :: LC -> Bool
isWHNF t | isRedex t = False
isWHNF (Abs t) = True
isWHNF (App f a) = isWHNF f
isWHNF _ = True
```

# A Lazy Solution

There are two solutions to the problem presented by the asymmetric benefits of the competeing evaluation orders. The first is crude, but an existent pattern in most languages that support lambdas, it is manually delayed evaluation using λ-abstraciont; thunks. The second solution is more robust but requires built in language support, it is referred to as lazy evaluation.

A thunk, for those of you who do not know, is a method for delaying evaluation by wrapping an expression in an extra layer of λ-abstraction. What is equally important in explanations of thunks is that, this style does not just require the values to change, but also the consumers of those values (who must now explicitly evaluate the expressions). The simplest example of a thunk would be this:
```
1. (λa.a)(λb.b)        -- This would be immediately evaluated
2. λdummy.(λa.a)(λb.b) -- the evaluation of the internal redex is deffered
```

Let's see how this can help us avoid non-termination in the **Applicative Order** evaluation example used in the previous section (NOTE: I use the name `dummy` to indicate a thunk, basically I don't intend on ever using this variable):

```
(λf.(λs.f(s s))(λs.f(s s)))(λx.y.y) -- Before the use of thunks
1. (λf.(λs.f(λdummy.(s s))(λs.f(λdummy.(s s))))(λx.y.y) =>
2. (λs.(λx.y.y)(λdummy.(s s))(λs.(λx.y.y)(λdummy.(s s)))) =>
3. (λx.y.y)(λdummy.((λs.(λx.y.y)(λdummy.(s s)))(λs.(λx.y.y)(λdummy.(s s))))) =>
2. (λy.y)
```
because of the `λdummy` lambda abstraction in the way, the argument to `λx.y.y` cannot be evaluated any further, and this λ-expression terminates. Let's look at a slightly different example to see how the function we pass to the higher-order recursive function `(λs.f(λdummy.(s s))(λs.f(λdummy.(s s))))` must now choose to explicitly evaluate the thunks. In the first example below I will substitute `λx.y.y` for `λx.y.x` a λ-expression that does not terminate, even under **Normal Order** evaluation, when the recursive function is applied to it. However, with the extra layer of thunks, we safely reach a **Head Normal Form**. This is because the function `λx.y.x` does not explicitly evaluate the recursive function. Below the dashed line we can see what happens when we use a function that explicitly evaluate the thunk, `λx.y.x <λ-expr>`, which applies the value `<λ-expr>` (a stand in for any λ-term) to the recursive function to keep it recursing.
```
1. (λf.(λs.f(λdummy.(s s)))(λs.f(λdummy.(s s))))(λx.y.x)                                             =>
2. (λs.(λx.y.x)(λdummy.(s s)))(λs.(λx.y.x)(λdummy.(s s)))                                            =>
3. (λx.y.x)(λdummy.((λs.(λx.y.x)(λdummy.(s s)))(λs.(λx.y.x)(λdummy.(s s)))))                         =>
4. λy.(λdummy.((λs.(λx.y.x)(λdummy.(s s)))(λs.(λx.y.x)(λdummy.(s s)))))
-------------------------------------------------------------------------------------------------------

-- Instead of <λ-expr> I will use the identity function λz.z
1. (λf.(λs.f(λdummy.(s s)))(λs.f(λdummy.(s s))))(λx.y.x(λz.z))                                       =>
2. (λs.(λx.y.x(λz.z))(λdummy.(s s)))(λs.(λx.y.x(λz.z))(λdummy.(s s)))                                =>
3. (λx.y.x(λz.z))(λdummy.((λs.(λx.y.x(λz.z))(λdummy.(s s)))(λs.(λx.y.x(λz.z))(λdummy.(s s)))))       =>

-- Now that we have the extra λz.z on the outside, evaluation can continue
4. λy.(λdummy.((λs.(λx.y.x(λz.z))(λdummy.(s s)))(λs.(λx.y.x(λz.z))(λdummy.(s s)))))(λz.z)            =>
5. λy.((λs.(λx.y.x(λz.z))(λdummy.(s s)))(λs.(λx.y.x(λz.z))(λdummy.(s s))))                           =>
6. λy.((λx.y.x(λz.z))(λdummy.((λs.(λx.y.x(λz.z))(λdummy.(s s)))(λs.(λx.y.x(λz.z))(λdummy.(s s))))))  =>
7. λy.(y.(λdummy.((λs.(λx.y.x(λz.z))(λdummy.(s s)))(λs.(λx.y.x(λz.z))(λdummy.(s s)))))(λz.z))        =>
8. λy.(y.((λs.(λx.y.x(λz.z))(λdummy.(s s)))(λs.(λx.y.x(λz.z))(λdummy.(s s)))))                       =>
...
n. λy.(y.(y.(λdummy.((λs.(λx.y.x(λz.z))(λdummy.(s s)))(λs.(λx.y.x(λz.z))(λdummy.(s s)))))(λz.z)))
...
```

We can see that the thunk style does not prevent non-termination, but allows us to evaluate some λ-expressions that would not normally terminate under **Applicative Order** evaluation. Lazy Evaluation accomplishes the same thing, but through and entirely different evaluation strategy. Lazy Evaluation only evaluates a λ-expression when it is in the function position ie. `<function position> <argument postion>`. The crux of lazy evaluation is that it requires keeping a reference to λ-expressions that have been substituted for the same bound variables, and then once one of those expressions is evaluated, that value is substituted for the rest.
