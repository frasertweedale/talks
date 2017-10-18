..
  Copyright 2015  Fraser Tweedale.

  This work is licensed under the Creative Commons Attribution 4.0
  International License. To view a copy of this license, visit
  http://creativecommons.org/licenses/by/4.0/.


************
Introduction
************

This talk
=========

- Introduce *property-based testing*; motivate with examples

- Concepts will be demonstrated in Haskell using *QuickCheck_*

- A brief look at property-based testing in other languages

- Discussion of limitations

- Alternative approaches

.. _QuickCheck: http://www.cse.chalmers.se/~rjmh/QuickCheck/


Property-based testing
======================

#. state **properties** of functions
#. declare how to **generate** arbitrary values
#. framework attempts to **falsify** properties
#. report **counterexamples**


Applications
============

- Check laws and invariants of algorithms, data, abstractions

- Check code against a model implementation

- Properties are meaningful documentation


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


*******
Gotchas
*******

Exhaustion
==========

- Naïve use of preconditions resulting in not enough test cases

- Solution: custom generator to ensure precondition satisfied

- Better solution: redesign data types such that precondition is
  invariant


Trivial test data
=================

- Trivial test data can result in tests passing *vacuously*.

- Use ``collect`` or ``cover`` to inspect distribution

- Use ``frequency`` to govern distribution


Infinite data structures
========================

- Useful, but be careful what you evaluate

- Use ``sized`` when defining generators for recursive data



***************
Other languages
***************

Property-based testing implementations
======================================

- Most languages have at least one implementation

- List: https://en.wikipedia.org/wiki/QuickCheck

.. _pyqcy: https://pypi.python.org/pypi/pyqcy
.. _Functional Java: http://www.functionaljava.org/

Java example
============

- *junit-quickcheck*
- https://github.com/pholser/junit-quickcheck/

.. code:: java

  static <A> List<A> rev(List<A> xs);
  static <A> List<A> app(List<A> xs, List<A> ys);

Java example
============

.. code:: java

  import static org.junit.Assert.*;
  import org.junit.contrib.theories.*;
  import org.junit.runner.RunWith;
  import com.pholser.junit.quickcheck.ForAll;

  @RunWith(Theories.class)
  public class RevTestCase {
    // next slide
  }

Java example
============

.. code:: java

  @Theory public void revUnit(@ForAll Integer x) {
    ArrayList xs = new ArrayList();
    xs.add(x);
    assertEquals(rev(xs), xs);
  }

  @Theory public void revApp(
    @ForAll ArrayList<Integer> xs,
    @ForAll ArrayList<Integer> ys
  ) {
    assertEquals(
      rev(app(xs, ys)),
      app(rev(ys), rev(xs))
    );
  }


Python example
==============

- *Hypothesis*



***********
Limitations
***********

Bugs
====

- Incorrect ``Arbitrary`` instances

- Incorrect properties

- Incomplete properties


Randomness
==========

.. code:: haskell

  prop_verify_eq :: Password -> Bool
  prop_verify_eq s = verify (hash s) s

  prop_verify_neq :: Password -> Password -> Property
  prop_verify_neq s s' =
    not (s == s')  ==>
      not (verify (hash s) s')


Randomness
==========

- Previous slide: what if ``hash`` truncates input before hashing?

- Some bugs are unlikely to be found with random data

- Workaround: mutate or fuzz data in domain-relevant way


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

- How to specify behaviour given *invalid* data?


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

- Check that property holds for all values

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

- Properties become theorems; no proof, no program

- Program *extraction* to other languages

- Completeness proofs

  - ``rev`` example: http://is.gd/EhanO1


Resources
=========

- *QuickCheck: A Lightweight Tool for Random Testing of Haskell
  Programs* (2000) Koen Claessen, John Hughes: http://is.gd/mpsY7G

- *Automated Unit Testing your Java using ScalaCheck* by Tony
  Morris: http://is.gd/j0R7qq

- UCSD CSE 230 lecture: http://is.gd/0YfxOr

- *QuickCheck: Beyond the Basics* by Dave Laing: http://is.gd/pGKnhg

- Recommended Haskell learning path:
  https://github.com/bitemyapp/learnhaskell


**********
Questions?
**********

Thanks for listening
====================

Copyright 2015, 2017  Fraser Tweedale

This work is licensed under the Creative Commons Attribution 4.0
International License. To view a copy of this license, visit
http://creativecommons.org/licenses/by/4.0/.

Email
  ``ftweedal@redhat.com``
Twitter
  ``@hackuador``
