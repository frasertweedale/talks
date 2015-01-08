..
  Copyright 2015  Fraser Tweedale.

  This work is licensed under the Creative Commons Attribution 4.0
  International License. To view a copy of this license, visit
  http://creativecommons.org/licenses/by/4.0/.


************
Introduction
************

About me
========

- Developer at Red Hat

- FreeIPA identity management and Dogtag PKI

- Mostly Python and Java at work

- Mostly Haskell for other projects


This talk
=========

- Introduce *property-based testing*; motivate with examples

- Concepts will be demonstrated primarily in Haskell using
  QuickCheck_

- A brief look at property-based testing in other languages

- Discussion of limitations

- Alternative approaches

.. _QuickCheck: http://www.cse.chalmers.se/~rjmh/QuickCheck/


Property-based testing
======================

A property-based testing framework:

#. Gives you a way to state properties of functions
#. Gives you a way to declare how to generate arbitrary values of
   your types
#. Provides generators for standard types (usually)
#. Attempts to falsify your properties and reports counterexamples.


Applications
============

- Check laws and invariants of algorithms, data, abstractions

  - These exist for pretty much everything in programming

  - Find out what they are, write them down; now you have tests

- Checking code against a model implementation

  - Does ``myFancySort`` have the same behaviour as stdlib ``sort``?

- Properties are useful documentation


********
Examples
********

Reversing a list
================

.. code:: haskell

  rev :: [a] -> [a]
  rev []     = []
  rev (x:xs) = rev xs ++ [x]

  prop_RevUnit :: Int -> Bool
  prop_RevUnit x =
    rev [x] == [x]

  prop_RevApp :: [Int] -> [Int] -> Bool
  prop_RevApp xs ys =
    rev (xs ++ ys) == rev ys ++ rev xs


Expression transformation
=========================



Gotchas
=======

- Naïve use of preconditions can result in not enough test
  cases.

  - Use custom generator to ensure precondition satisfied.

  - Redesign your types so that precondition is an invariant.

- Trivial test data can result in tests passing *vacuously*.

  - Generated data can be *classified* to guard against this.

  - Leverage mechanisms to control distribution.

- Be careful not to generate infinite data.

  - Use ``sized``.

- Like other kinds of tests, properties and generators can have
  bugs.


***************
Other languages
***************

Property-based testing implementations
======================================

- Most languages have at least one implementation

- Incomplete list at https://en.wikipedia.org/wiki/QuickCheck

- Some decent or popular implementations are missing
  - Python: pyqcy_
  - Java: `Functional Java`_ (``fj.test``)

.. _pyqcy: https://pypi.python.org/pypi/pyqcy
.. _Functional Java: http://www.functionaljava.org/


Python example
==============

.. code:: python

  from pyqcy import *

  def rev(l):
      return list(reversed(l))

  @qc
  def prop_rev_unit(x=int_()):
      assert rev([x]) == [x]

  @qc
  def prop_rev_app(xs=list_(of=int), ys=list_(of=int)):
      assert rev(xs + ys) == rev(ys) + rev(xs)

  if __name__ == '__main__':
      main()


***********
Limitations
***********

Randomness
==========

.. code:: haskell

  prop_verify_eq :: Password -> Bool
  prop_verify_eq s = verify (hash s) s

  prop_verify_neq :: Password -> Password -> Property
  prop_verify_neq s s' =
    s /= s' ==> not $ verify (hash s) s'


Randomness
==========

- Some bugs are unlikely to be found with random data

- Previous slide: what if ``hash`` truncates input before hashing?

  - Unlikely that random strings will have long common prefix

- Workaround: mutate or fuzz data in domain-relevant way

  - Assumes programmer knowledge / cleverness (not ideal)


Randomness
==========

.. code:: haskell

  fuzz :: Password -> Gen Password
  fuzz = {- truncation / extension / permutation / etc -}

  prop_verify_fuzzed :: Password -> Property
  prop_verify_fuzzed s =
    forAll (fuzz s) (prop_verify_neq s)


Failure cases
=============

- ``Arbitrary`` is great for generating random *valid* data

- Examples are usually appropriate for specifying behaviour given
  *invalid* data.


Failure cases
=============

.. code:: haskell

  dump :: JSON   -> String
  load :: String -> Maybe JSON

  prop_dumpLoad :: JSON -> Bool
  prop_dumpLoad a = load (dump a) == Just a

  loadSpec :: Spec
  loadSpec = describe "load" $
    it "fails on bogus input" $
      load "bogus" `shouldBe` Nothing


Conclusion
==========

- Property-based testing is *true automated testing*

  - More thorough testing in less time ($$$)

  - Relieves developer of burden of finding and manually writing
    tests for corner cases

- Properties are *meaningful documentation*

- *The best test data is random test data*, but...

  - a bit of domain-specific non-randomness is sometimes useful

  - examples still have their place.


**********************
Alternative approaches
**********************

Exhaustive testing
==================

*The best test data is all of the data*

- Check that property holds for all values (up to a certain size)

- Supports *existential* properties

- Available in several languages

  - SmallCheck_ (Haskell),
    smallcheck4scala_,
    autocheck_ (C++),
    ocamlcheck_,
    `python-doublecheck`_

.. _SmallCheck: http://hackage.haskell.org/package/smallcheck
.. _smallcheck4scala: https://github.com/dwhjames/smallcheck4scala
.. _autocheck: https://github.com/thejohnfreeman/autocheck
.. _ocamlcheck: https://github.com/jamii/ocamlcheck
.. _python-doublecheck: https://github.com/kennknowles/python-doublecheck


Proof
=====

*The best test data is no test data*

- Some languages have theorem-proving capabilities
  - Coq, Idris, Agda and others

- Declare properties as theorems and prove them
  - No proof, no program

- Some proof assistants support *extraction* to other languages


Proof - Idris example
=====================

.. code:: idris

  rev : List a -> List a
  rev [] = []
  rev (x :: xs) = rev xs ++ [x]

  revUnit : rev [a] = [a]
  revUnit = Refl

  revAppend :  (xs, ys : List a)
            -> rev (xs ++ ys) = rev ys ++ rev xs
  revAppend = ?proof_revAppend


Proof - Idris example
=====================

.. code:: idris

  proofRevAppend = proof
    intros
    induction xs
    compute
    rewrite (appendNilRightNeutral (rev ys))
    trivial
    intros
    compute
    rewrite sym ihl__0
    rewrite (appendAssociative (rev ys) (rev l__0) [t__0])
    trivial


Resources
=========

- *QuickCheck: A Lightweight Tool for Random Testing of Haskell
  Programs* (2000) Koen Claessen, John Hughes
  http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.47.1361

- UCSD CSE 230 lecture:
  https://cseweb.ucsd.edu/classes/wi12/cse230-a/lectures/quickcheck.html

- *Automated Unit Testing your Java using ScalaCheck*
  http://tonymorris.github.io/blog/posts/automated-unit-testing-your-java-using-scalacheck/

- Learn Haskell: https://github.com/bitemyapp/learnhaskell


Thanks for listening
====================

Copyright 2015  Fraser Tweedale

This work is licensed under the Creative Commons Attribution 4.0
International License. To view a copy of this license, visit
http://creativecommons.org/licenses/by/4.0/.

Slides
  https://github.com/frasertweedale/talks/
Email
  ``frase@frase.id.au``
Twitter
  ``@hackuador``


*********
Questions
*********
