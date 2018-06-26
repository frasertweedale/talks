Intro
=====

- I work in applied cryptography
- A few years ago, the IETF formed the *jose* WG
- Something I was interested in used JOSE, so I implemented it

- This talk is a story about all the lessons I've learned from being
  involved in the development of JOSE and 


Do you need another standard?
=============================

TAKEAWAY: writing libraries > writing standards

- jose wg charter: https://tools.ietf.org/wg/jose/charters

..
  With the increased usage of JSON in protocols in the IETF and
  elsewhere, there is now a desire to offer security services, which use
  encryption, digital signatures, message authentication codes (MACs)
  algorithms, that carry their data in JSON format.

- Use cases RFC: https://tools.ietf.org/html/rfc7165 

..
   Many current applications thus have much more robust support for
   processing objects in these text-based formats than ASN.1 objects;
   indeed, many lack the ability to process ASN.1 objects at all.  To
   simplify the addition of object-based security features to these
   applications, the IETF JSON Object Signing and Encryption (JOSE)
   working group has been chartered to develop a secure object format
   based on JSON.


Assumptions
-----------

- ASN.1 libraries don't exist or can't be used

- It's better to define a new standard than write an ASN.1 library

- JSON is a suitable wire format for cryptographic objects


Is JSON the right choice?
=========================

What is JSON?
-------------

- Good question

- Doug Crockford: https://www.youtube.com/watch?v=-C-JoyNuQJs
  - "discovered JSON", not invented.  Found it, named it, described
    it (2001).  He was not the first.

- https://tools.ietf.org/html/rfc4627 (Jul 2006)
- https://tools.ietf.org/html/rfc7159 (Mar 2014)
- https://tools.ietf.org/html/rfc8259 (Dec 2017)

- Originally, it was de facto subset of JavaScript for specifying
  data.

Falsehoods programmers believe about JSON
-----------------------------------------

- JSON is a subset of JavaScript

- JSON is unambiguously specified
  http://seriot.ch/parsing_json.php §2.6
  - invalid unicode code points are accepted by the grammar
    and "MUST be parsed"
  - implementations may set limits on size of data, but must also
    parse them?

- JSON objects are hash maps

- You can use duplicate keys for "comments"

- JSON support is universal

- JSON is human-readable

- JSON parsers have the same behaviour


Other problems with JSON
------------------------

- JSON has number
- Shipping binary data (need a JSON-compatible encoding e.g. base64)


Other options
-------------

- ASN.1 and its serialisation
  - Old, complex, but good tool and library support
  - data definitions can specify constraints

- CBOR
  - new, much simpler than ASN.1
  - not yet widespread support


Takeaway: JSON is not all that.  If you have to "work around" the
wire format, it is not the right wire format.



New format, old crypto?
=======================

- There's really no excuse for this.


Ambiguity and interoperability?
===============================

- TIMTOWTDI (Perl)
- There should be one—and preferably only one—obvious way to do it
- There is exactly one way to do it

- This is *not* the same as saying "there is only one way to use it"

- The creation of user-friendly interfaces for common use cases is
  then up to the implementor.

- But it already was up to them.

- And you've made their job easier because there is less ambiguity
  in the spec, and less stuff to implement

Dealing with ambiguity
----------------------

- Push ambiguity into the types; let the library user decide what
  they want.

- Example - flattened vs general JWS syntax

- An "optimisation" for the common case turned out be exactly the
  opposite.  It became an ambiguity that led to incompatibility
  between different implementations and programs using them.
  (https://github.com/frasertweedale/hs-jose/issues/26)

- Implementers should provide simple APIs for the common use
  case(s).  The implementation is the correct place to do that.

- TAKEAWAY: Never have special cases for common use cases in the
  spec itself.

  - Two options: ONLY handle the common use case, and exclude the
    others).  Or, handle ALL use cases the same way (in the spec).


Common vulnerabilities in JOSE libraries
========================================

Takeaway: don't use broken crypto.  Even if people say they want it.
What's the worst that would happen - they're stuck using old,
presumably battle-hardened implementations of older specs.  Not so
bad.  Or they can upgrade their HSMs and use the new stuff.

Talk about the NaCl approach of one version number, one fully
specified suite of primitives and algos.  Consider this approach.

Paseto:
- https://github.com/paragonie/paseto
- https://paragonie.com/blog/2018/03/paseto-platform-agnostic-security-tokens-is-secure-alternative-jose-standards-jwt-etc


Implementation: how to encourage or enforce safe use
====================================================

- TAKEAWAY: use type systems to enforce correct behaviour

- Example: (jose) don't provide access to unverified data

- Example: (jose) phantom type variable to indicate whether
           key contains private data


So you're going to write a new standard
=======================================

TAKEAWAY: Implement the formats/protocols you're specifying, before
the mistakes get locked in.  Multiple implementations.  Spot the
ambiguities.  Spot the interop problems.  Simplify.  Find and
eliminate ambiguities, special cases and edge cases.  Exclude
esoteric use cases.  You can't be all things to all people, so don't
try.  If you try, you will probably make life harder for the
majority.  It's OK to say "no!"


Conclusion
==========

- recap the takeaways
- Q&A
