How to configure a program
==========================

Author:
  Fraser Tweedale

Session format:
  talk

Audience:
  intermediate

Target audience:
  developers

Category:
  Technique


Abstract
--------

There are many ways to configure a program, ranging from the austere
(command line options or environment variables) to dedicated
configuration languages (YAML, Dhall, etc).  But one approach stands
apart from all others: the "xmonad way".  Why bother interpreting a
configuration?  Just recompile the program!

In this presentation, using the *purebred* mail program as an
example, with supporting code examples and live demonstrations, I
will explain the benefits and challenges of the xmonad approach to
program configuration.  In particular I will discuss:

- Why this is a principled and reasonable approach to configuration,
  and what kinds of programs benefit from it.

- Library support, in particular Haskell's *dyre* library.

- How to structure your types and modules for a pleasant user
  experience, and for strong guarantees of correctness and safety.

- Challenges in development environments (cabal-install, Stack) and
  distributions like NixOS.

- Drawbacks and barriers to general acceptance.

People developing user-configurable programs will get the most out
of this talk.  All programs, tools, libraries and code examples use
Haskell, but the technique is applicable in many languages.


Learning outcome
----------------

You will learn how to use the *dyre* library to configure Haskell
programs the xmonad way.  You will understand the tradeoffs and know
how to work around the challenges presented by some environments.
Users of languages other than Haskell may be challenged to pursue
this configuration paradigm in their language of choice.


Outline / structure
-------------------

- Overview and comparison of different approaches to configuration
- Benefits of the xmonad approach
- Library support (dyre)
- Live demo (purebred)
- Drawbacks of the xmonad approach
- Challenges in development and Nix environments, and workarounds
- Support in other languages
