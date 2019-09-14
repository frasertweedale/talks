JSON mason: building a JSON parser from scratch (live)
======================================================

Parser combinators are functions that construct parsers out of
smaller, simpler parsers.  Like LEGO, the building blocks are
generic and can be combined to build parsers for complex formats.
This approach facilitates readable, maintainable parsers and
promotes code reuse.  In this live coding presentation I'll
implement a JSON (subset) parser, and the combinator library used to
build it, from scratch!

We'll use Haskell, a simple and concise functional programming
language.  After defining what we want the parser to look like,
we'll drill down to a basic definition of a parser, and implement
the simplest possible parser.  From there we will define the
combinators we need, and complete the JSON parser.  Types and
abstractions will be explained as they are encountered.

The session will conclude with a brief discussion of considerations
for implementing or choosing a parser combinator library for real
world use.

Audience members will grasp parser combinators and their advantages,
and enhance their intution for some foundational programming
abstractions including functors and applicative functors.  All code
and steps will be published for the audience to follow along during
the presentation, or at their leisure.

Audience
--------

Developers

Private abstract
----------------

A parser library and JSON parser coded in 40 minutes, in front of an
audience, while explaining every step... seems crazy right?

But I've done this (or variations of it) before.  Let me address a
couple of failure modes:

1. Machine failure; short of a machine crashing during the
   presentation, it's not a big concern.  If my own machine is
   busted, I can get it done on any machine with a Haskell compiler
   and Vim installed.

2. I make a mistake and get stuck; definitely possible.  But type
   errors, rehearsal and past experience mitigate the risk.  Also,
   friendly audience members who spot the silly mistakes.

The presentation will be refined and rehearsed extensively to ensure
the time is used effectively, for maximum audience benefit and
allowing time for questions along the way.  The nice thing about
coding a subset of JSON is the size of the subset can be varied
(even on the fly).

I will make all the content available for audience members to follow
along.  Possibly also a Jupyter notebook (I have little experience
with Jupyter so I don't want to commit to that yet.)
