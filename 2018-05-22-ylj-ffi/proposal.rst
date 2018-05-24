Taming the C monster: Haskell FFI techniques
============================================

Author:
  Fraser Tweedale

Session format:
  talk

Audience:
  intermediate

Category:
  Case Study


Abstract
--------

Haskell has a powerful *foreign function interface (FFI)* for
interfacing with C libraries.  Haskell is a great language for
building libraries and tools, but interoperability requirements or
time constraints can make the FFI a compelling option.

Binding to a non-trivial C library presents several challenges
including C idioms, memory management, error handling and more.
This presentation will address a selection of these concerns, using
*hs-notmuch*[1], a binding to the *notmuch* mail indexer, as a case
study.  We will discuss:

- FFI basics and tools to assist binding authors

- working with "double pointer"-style constructors

- working with iterators; how to do lazy iteration

- how to use Haskell's garbage collector to manage lifecycles of
  external objects, and "gotchas" encountered

- using types to enforce correct use of unsafe APIs

- performance considerations (including profiling results)

The presentation will conclude with a mention of some important FFI
concepts that were not covered (e.g. callbacks) and a look at how
*hs-notmuch* is being used in the Real World.

Developers familiar with C will get the most out of this talk
(because there will be limited time to explain C idioms, memory
management, etc).  To varying degrees, most of the concepts and
techniques discussed will apply to other languages' FFIs.

[1] https://github.com/purebred-mua/hs-notmuch


Learning outcome
----------------

Attendees will learn Haskell FFI techniques for dealing with some
common C idioms, memory management and unsafe APIs.  They will be
confident to write bindings to many kinds of C libraries, and will
be aware of some of the gaps that this presentation did not cover.
