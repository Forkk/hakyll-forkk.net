---
title: Haskell is Beautiful
author: Forkk
date: 1400362048000
---

Haskell is probably one of my favorite programming languages, but
when I try to explain some of the concepts in Haskell to people, they don't
seem to get it. It's not that they don't understand what I mean when I say that
functions have no side effects. They just don't seem to understand *why* functions
have no side effects. They don't understand *why* everything has to return some
value, and they certainly don't understand *why* monads even exist.

In reality, though, functions in Haskell *shouldn't* have side effects, everything
*should* return a value, and Haskell would be nearly useless without monads. You
just need to change how you think about Haskell and suddenly all of these rules
go from arbitrary to obvious.


# Paradigm Shift

Understanding Haskell requires a paradigm shift. Haskell programs are not sequences
of instructions, but rather, expressions to be evaluated. Everything in Haskell
is an expression. Once you understand this, you can begin to see how all of these
seemingly arbitrary rules totally make sense.


# No Side Effects

In Haskell, functions have no side effects. This simply means that a function
cannot access or manipulate any global state. In fact, there is no global state.
The only variables a function accesses are its arguments.

In an imperative language, this concept makes no sense. Functions in an imperative
language aren't expressions, they're blocks of code and you call them to execute
them. Of course they'd be able to have side effects. Not allowing side effects in
an imperative language would just be a silly arbitrary restriction.

If you think in terms of mathematical expressions, however,  this concept makes sense.
Of course functions can't have side effects! They're just expressions. All
they do is take some input an give a result. Why would they go out and manipulate
some global state?


# All Functions Return Values

Another confusing concept is the fact that all functions must return values. This
is simple enough to understand, but it makes no sense in an imperative language.
What about functions that don't *need* to return anything. If I have a function
that goes and prints text to the terminal, what does that return? How could that
ever return any meaningful value other than some error code?

Once again, this concept makes sense when everything is an expression. Of course
all functions return values! They're just expressions. Furthermore, how would
any function do anything without being able to return some value. Since there's
no global state, how do we do anything without returning a value?


# Monads

This is the scary one. The ever frightening monad. Some people seem to think monads
are unnecessary and/or confusing. In reality, however, monads tie all of our beautiful
functions together quite nicely and resolve a lot of issues that the afforementioned
"rules" would otherwise cause.

For example, how do we do IO if there are no side effects?
How do we interact with the outside world?
What about functions that need to carry around some sort of state?

Monads are the obvious solution to all of these problems. There are many ways
to think of them, but I like to think of them as action values. For example,
what is the return value of a function that prints to a terminal anyway? Well,
the `putStrLn` function's return value is actually `IO ()`. We can call that
an IO *action*.

An IO action simply represents some sort of action to be done involving IO.
For example, `putStrLn` represents an IO action which prints a string. If
we want to actually execute this action, we simply return it from our program's
`main` function like so:

    main = putStrLn "Hello World"

We can also chain multiple IO actions into one using the bind (`>>`) function.
The bind function simply creates a new IO action which executes the left hand
side IO action and then executes the right hand side IO action.

    main = putStrLn "Hello" >> putStrLn "World"

Now, we can construct our program out of chains of these IO actions. Bind doesn't
just work for IO actions, though. It works for any monad. There are lots of other
monads, but that's something that's far out of the scope of this post. Monads allow
us to build sequences of instructions as expressions by chaining actions together
with bind. They tie everything together and make Haskell useful.


# Haskell is Beautiful

In the end, all of these confusing rules in Haskell make sense. It would be
silly without them. You just need to look at it the right way. Yes, Haskell
is weird, but that's why it's so beautiful.
