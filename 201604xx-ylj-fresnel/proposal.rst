Unified Parsing and Printing with Prisms
========================================

Author:
  Fraser Tweedale

Session format:
  talk

Audience:
  intermediate

Keywords:
  parsing, lens, prism, haskell

Abstract
--------

Parsers and pretty printers are commonly defined as separate values,
however, the same essential information about how the structured
data is represented in a stream must exist in both values.  This is
therefore a violation of the DRY principle - usually quite an
obvious one (a cursory glance at any corresponding ``FromJSON`` and
``ToJSON`` instances suffices to support this fact).  Various
methods of unifying parsers and printers have been proposed, most
notably *Invertible Syntax Descriptions* due to Rendel and Ostermann
(several Haskell implementations of this approach exist).

In this talk attendees will learn an alternative approach to unified
parsers and printers based on a familiar abstraction: *prisms*.  We
begin with an abstract parser definition based on the ``Cons`` type
class (part of the *lens* library).  The underlying prism gives rise
to the functions ``uncons``, whose type matches that of a "typical"
functional parser, and ``cons``, the dual (a printer!)  From there
we will examine how the `*fresnel*
<https://github.com/frasertweedale/hs-fresnel>`_ library uses these
building blocks to implement a combinator library for building
parser/printer prisms out of existing prisms and isos.

We will see how *fresnel* has been applied in an ASN.1/DER library
to correctly handle some of DER's "special" encoding requirements.
Finally, I will discuss some shortcomings of prism-based
parser/printers, including the lack of useful error reporting.

