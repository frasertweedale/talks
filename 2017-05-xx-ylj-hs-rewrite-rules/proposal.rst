Performant polymorphism: rewrite rules in Haskell
=================================================

Author:
  Fraser Tweedale

Session format:
  talk

Audience:
  intermediate

Keywords:
  haskell, ghc, optimisation, performance, polymorphism


Abstract
--------

GHC usually does an excellent job of transforming well written
Haskell code into efficient machine code, but sometimes "fast" is
not "fast enough".  Common optimisation techniques when dealing with
concrete data types often do not apply to polymorphic data and
functions.  A concise, generic algorithm may perform poorly for some
types, but providing a faster version with a less polymorphic type
sacrifices reusability and parametricity!  What's a principled
programmer to do?

Fortunately GHC has got your back here, too.  In this talk we will
learn about GHC's *rewrite rules* feature, which can be used for
substituting alternative, better performing implementations of
polymorphic functions at particular (less polymorphic) types,
without changing the type signature that users see, preserving reuse
and parametricity.  We will see also how to define transformation
rules that employ theorems (free or otherwise) to optimise programs.

We will also briefly examine how rules are applied by observing the
firing of rules and changes effected in the produced Core (GHC's
fully desugared intermediate language), and see how to control the
order of inlining and rewrite rules to achieve the desired
outcome.

Finally, we'll look at a real-world example of how rewrite rules are
used in the 'fresnel'[1] library, a unified parser-printer
combinator library based on the 'Cons' abstraction from the 'lens'
library, to dramatically speed up printing for certain output types.

This will be a hands-on talk with live coding, benchmarking and
profiling (no optimising without metrics!) and Core spelunking.
Audience members familiar with Haskell should expect to learn some
basics of Haskell benchmarking and profiling, gain an understanding
of when and how to use rewrite rules in their own code, and
walk out feeling comfortable (or less trepidatious, perhaps) about
reading and analysing their programs' Core.

[1] https://github.com/frasertweedale/hs-fresnel
