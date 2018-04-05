attoparsec acrobatics
=====================

Author:
  Fraser Tweedale

Session format:
  talk

Audience:
  intermediate

Keywords:
  parsing, haskell

Abstract
--------

*attoparsec* is a high performance parser combinator library for
Haskell.  It is designed for ByteString parsing and sacrifices
helpful error messages and flexibility in the name of speed.  Or so
the story goes.  It turns out that there is more flexibility in
attoparsec than meets the eye.

This presentation will familiarise attendees with attoparsec's
internals and the design goals that led to the current
implementation.  This will provide an understanding of how
attoparsec achieves its high performance, how to get the most out of
it and constructions to avoid.

Following this, we will discuss two parser use cases:

- reporting the position at which parsing failed (often held to be
  impossible with attoparsec)

- chunking an input by a fixed delimiter string (the naïve
  implementation is inefficient)

We will see how attoparsec's internals can be exploited to satisfy
these use cases.  Along the way, some other interesting parsers will
be discovered.  We will also discuss how the efficient chunking
combinator could be constructed in other libraries.  The performance
impact of the efficient chunking combinator for MIME parsing in the
*purebred-email* [1] library will be demonstrated.

This presentation assumes familiarity with Haskell syntax and parser
combinator library usage.  Concepts are applicable in other
libraries and languages.

[1] https://github.com/purebred-mua/purebred-email


Learning outcome
----------------

Attendees will understand attoparsec internals and know how to get
the best performance out of a parser.  They will see that the
implementation affords many useful but non-obvious parsers.  Ideas
about what is possible with a particular parser library may be
challenged.
