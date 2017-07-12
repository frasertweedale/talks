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
has fallen.  Today, although the tech world remains littered with
the legacies of fallen formats, developers across the world pay
tribute to JSON, the *linuga franca* for data exchange on the web.

Is JSON a powerful, glorious format, worthy of developers'
allegiance?  Or is the feet of clay: substandard and brittle?

In this talk, I will identify and discuss some profound issues with
JSON, focusing on ambiguities in the JSON standard, inconsistencies
between implementations and shortcomings in JSON's data model that
hinder accurate representation of some kinds of data.  The *JSON
Object Signing and Encryption (JOSE)* standards, which are used in
the Let's Encrypt *ACME* protocol, *OpenID Connect* and elsewhere,
will be used to demonstrate some of the problems.  I will also
debunk some common arguments in favour of JSON including *"JSON is
human-readable / easier for debugging"* and *"JSON is supported
everywhere / more places than other formats"*.

We will conclude with an overview of some forsaken and emerging
serialisation formats including *ASN.1* and *CBOR*, and why you
might prefer them over JSON.  We'll also discuss some alternative
techniques for data transfer and common mistakes to avoid when
modelling your data with serialisation in mind.


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
