..
  This work is licensed under the Creative Commons Attribution 4.0
  International License. To view a copy of this license, visit
  http://creativecommons.org/licenses/by/4.0/.

Yorgey Lecture 5 by Fraser Tweedale
===================================

All programmers know that different data types can have common
attributes.  Apples and oranges may not be comparable, but they both
have weight, can be peeled, and so on.  And if you had a basket of
fruit you might want to count the fruit or sort items by weight.
Even though the basket could be full of apples, oranges or even
bananas, the same operations apply.

This month's lecture covers *parametric polymorphism* and *type
classes*.  We will see how *parametricity* provides strong
guarantees about the behaviour of a function, and how type classes
can be used to define functions that apply to disparate types.  Type
classes will be compared to classes in other languages, and we will
meet some of Haskell's common type classes and learn how to use them
with our types, as well as how to define new type classes.
