Infinite scroll: lazy lists in the Brick TUI library
====================================================

Author:
  Fraser Tweedale

Session format:
  talk

Audience:
  intermediate

Target audience:
  developers

Category:
  Case study


Abstract
--------

The *Brick* terminal UI library provides a rich library of widgets
for building console applications in Haskell.  These include a list
widget, which uses a packed vector type under the hood: a major
problem when working with lists that are very large or expensive to
compute.

In this case study I will review, step by step, how I generalised
Brick's list widget to admit different underlying container
structures and achieved lazy loading of list items.  The
presentation will address several topics including:

- The advantages of more general (polymorphic) code, including
  parametricity

- Ensuring adequate test coverage before refactoring or generalising

- Maintaining backwards compatibility

- Assessing and documenting asymptotic performance

- Using Brick list widget in a real-world application (purebred MUA)
  for lazy loading where I/O is involved

- How to evaluate a lazy structure in the background (and why you
  might want to).

- Can we *really* achieve infinite scroll, or is my presentation
  title just clickbait?

Code examples will abound, and live demonstrations will both justify
the work that was done, and show the pleasing results.  The
presentation uses Haskell exclusively but principles and advice for
generalising code apply to many languages.


Learning outcome
----------------

Attendees will learn the benefits of, and general guidelines for
generalising code.  They will be familiar with the Brick list widget
implementation and know how to choose (or define) the right
container type to use with it.  A firm grasp on laziness and lazy
I/O in Haskell.


Outline / structure
-------------------

- Overview of Brick and the list widget
- Live demo: the performance issue
- The generalisation work, in detail
- Live demo: the results
- Concurrent evaluation of lazy structures
- Live demo: concurrent evaluation
- Infinite scroll
