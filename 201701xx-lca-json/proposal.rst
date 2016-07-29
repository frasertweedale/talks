JSON: Feet of Clay
==================

Target audience: Developer


Abstract
--------

Nebuchadnezzar, king of Babylon, dreamed of a great statue with a
head of gold, silver chest, brass belly, legs of iron and feet of
clay.  The prophet Daniel gave the interpretation of the dream: the
head of gold was Babylon, yet Babylon would fall and would be
followed by successively inferior kingdoms.

Serialisation formats, like empires, rise and fall.  ASN.1 once had
a great kingdom; it lives on today as a mere echo of its former
glory.  After ASN.1 came XML which ruled a great realm, but it too
has fallen.  Today, though the tech world remains littered with the
legacies of fallen formats, developers across the world pay tribute
to JSON.

The feet of clay are JSON.

In this talk, I will identify and explain some profound issues with
JSON, focusing on ambiguities in the JSON standard, inconsistencies
between implementations and shortcomings in JSON's data model that
cause difficulties in accurately representing particular kinds of
data.  The JSON Object Signing and Encryption (JOSE) standards,
which are used in the *Let's Encrypt* ACME protocol and elsewhere,
will be used as a case study to demonstrate some of the problems.  I
will also address some common arguments in favour of JSON including
"JSON is human-readable / easier for debugging" and "JSON is
supported everywhere / more places than other formats".

We will conclude with a brief overview of some forsaken or emerging
serialisation formats, or alternative techniques, that you might
consider using instead of JSON, and some mistakes to avoid when
modelling your data for serialisation.


Private abstract
----------------

Although the talk is not focused on any particular project or
library, we will observe implementation issues and inconsistencies
in a number of open source JSON implementations.  The presenter's
experiences implementing the *jose* library for Haskell will also be
used to point out problems with JSON in some domains.

This talk does not address performance, rather it is about
correctness.  It is the presenter's opinion that performance
differences between serialisation formats (implementations thereof)
are a marginal concern for an overwhelming majority of developers.
