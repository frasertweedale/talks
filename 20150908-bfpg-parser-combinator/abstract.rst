Parser combinators from scratch
===============================

Parser combinators - functions to assemble big parsers from little
ones - are an instructive example of composability and reuse made
possible through FP, and building one is a useful (and common!)
exercise for learners seeking through practical application a solid
understanding of abstractions such as applicative functors and
monads.

In this talk, we will briefly examine some parsing libraries
available for Haskell and discuss their relative benefits and
drawbacks.  Then we will ignore everything we just saw and build a
parser combinator library from scratch.  Moreover, our parser will
be abstracted over the "whole" and "element" input types - a more
general result than most parsers.

I will conclude with a summary of some topics for further study
including important parser design decisions, optimisations and
efforts to unify parsing and pretty printing.
