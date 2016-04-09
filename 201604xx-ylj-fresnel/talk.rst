..
  Copyright 2016  Fraser Tweedale

  This work is licensed under the Creative Commons Attribution 4.0
  International License. To view a copy of this license, visit
  http://creativecommons.org/licenses/by/4.0/.

.. notes:

  - talk slot: 30 minutes *including intro and questions*

  - all type class constraints should be in parens, for consistency

*****
Intro
*****

Background
==========

.. role:: latex(raw)
   :format: latex

.. role:: haskell(code)
   :language: haskell

- ASN.1

- DER is "special"

- Existing libraries fell short

.. class:: notes

  - Confession:
    - I hardly know anything about parsing
    - But I know I don't like repeating myself
    - And I know a bit about APIs and abstraction


Assumed knowledge
=================

- Haskell syntax

- "Traditional" functional parser combinators


Prism primer
============

.. code:: haskell

  ghci> :t _Right
  _Right :: Prism (Either c a) (Either c b) a b

.. code:: haskell

  ghci> preview _Right (Right 42)
  Just 42

.. code:: haskell

  ghci> preview _Right (Left "error")
  Nothing

.. code:: haskell

  ghci> review _Right 42
  Right 42

.. code:: haskell

  ghci> preview (_Left . _Right) (Left (Right 42))
  Just 42


..
  ASN.1 and DER
  =============

  ::

    GeneralSubtree ::= SEQUENCE {
      base              GeneralName,
      minimum    [0]    BaseDistance DEFAULT 0,
      maximum    [1]    BaseDistance OPTIONAL }


ASN.1 and DER
=============

::

  BasicConstraints ::= SEQUENCE {
    cA                 BOOLEAN DEFAULT FALSE,
    pathLenConstraint  INTEGER (0..MAX) OPTIONAL }


:haskell:`Data.X509.Ext`
========================

.. code:: haskell

  instance Extension ExtBasicConstraints where
    extDecode [Start Sequence,End Sequence] = ...
    extDecode [Start Sequence,Boolean b,End Sequence] = ...
    extDecode [Start Sequence,Boolean b,IntVal v,End Sequence] = ...
    extDecode _ = Left "unknown sequence"


*******
Parsers
*******


Parser
======

.. code:: haskell

  newtype Parser a = Parser
    { runParser :: String -> Maybe (a, String) }

  char :: Parser Char
  char = Parser $ \s -> case s of
    ""      -> Nothing
    (c : s) -> Just (c, s)

..

  Parser - example
  ================

  .. code:: haskell

    data Thing = Thing Int Bool

    parseThing :: Parser Thing
    parseThing = Thing <$> parseInt <*> parseBool


What if...
==========

- input type is not :haskell:`String`?

- *element* type is not :haskell:`Char`?


``Cons``
========

.. code:: haskell

  uncons
    :: (Cons s s a a)
    => s -> Maybe (a, s)  -- look familiar?


``Cons``
========

.. code:: haskell

  class Cons s t a b where
    _Cons :: Prism s t (a, s) (b, t)

  instance Cons ByteString ByteString Word8 Word8
  instance Cons Text Text Char Char
  instance Cons [a] [b] a b
  instance Cons (Vector a) (Vector b) a b
  -- and many more!


Redefining the parser
=====================

.. code:: haskell

  type  Parser s a = Prism s s (a, s) (a, s)

  char    :: (Cons s s a a) =>  Parser s a
  char    = _Cons

  runParser  ::  Parser s a -> s -> Maybe (a, s)
  runParser  =   preview

   
   


But if the parser is a ``Prism``...
===================================

.. code:: haskell

  type  Parser s a = Prism s s (a, s) (a, s)

  char    :: (Cons s s a a) =>  Parser s a
  char    = _Cons

  runParser  ::  Parser s a -> s -> Maybe (a, s)
  runParser  =   preview

  runPrinter ::  Parser s a -> (a, s) -> s
  runPrinter =   review


Parser ∧ printer → grammar
==========================

.. code:: haskell

  type Grammar s a = Prism s s (a, s) (a, s)

  element :: (Cons s s a a) => Grammar s a
  element = _Cons

  runParser  :: Grammar s a -> s -> Maybe (a, s)
  runParser  =   preview

  runPrinter :: Grammar s a -> (a, s) -> s
  runPrinter =   review


Convenience functions
=====================

.. code:: haskell

  parse :: Grammar s a -> s -> Maybe a
  parse g s = fmap fst (preview g s)

  print :: (Monoid s) => Grammar s a -> a -> s
  print g a = review g (a, mempty)


Grammar - map
=============

.. code:: haskell

  (<<$>>) :: Prism a a b b -> Grammar s a -> Grammar s b
  p <<$>> g = g . swapped . aside p . swapped

  swapped :: Iso (a, b) (c, d) (b, a) (d, c)
  aside :: Prism s t a b -> Prism (e, s) (e, t) (e, a) (e, b)


Grammar - product
=================

.. code:: haskell

  (<<*>>) :: Grammar s a -> Grammar s b -> Grammar s (a, b)
  p1 <<*>> p2 = p1 . aside p2 . productIso
    where
    productIso = iso
      (\(a, (b, s)) -> ((a, b), s))
      (\((a, b), s) -> (a, (b, s)))


Grammar - sum
=============

.. code:: haskell

  (<<+>>) :: Grammar s a -> Grammar s b -> Grammar s (Either a b)
  p1 <<+>> p2 = prism'

    (\(x, s) -> either (review p1 . (,s)) (review p2 . (,s)) x)

    (\x ->     first Left  <$> preview p1 x
           <|> first Right <$> preview p2 x)


Grammar - list
==============

.. code:: haskell

  many :: Grammar s a -> Grammar s [a]
  many p = isoList <<$>> (p <<*>> many p) <<+>> success ()

  isoList :: Iso' (Either (a, [a]) ()) [a]
  isoList = ...

  success :: a -> Grammar s a
  success a = prism' snd (Just . (a,))


Grammar - basic grammars
========================

.. code:: haskell

  satisfy :: (Cons s s a a) => (a -> Bool) -> Grammar s a
  satisfy f = prism id
    (\a -> guard (f a) >> pure a) <<$>> element

  symbol :: (Cons s s a a, Eq a) => a -> Grammar s a
  symbol a = satisfy (== a)

  eof :: (Cons s s a a) => Grammar s ()
  eof = prism' snd
    (\s -> maybe (Just ((), s)) (const Nothing) (uncons s))


Grammar - delimiters, etc
=========================

.. code:: haskell

  literal :: (Cons s s a a, Eq a) => a -> Grammar s ()

  match :: Grammar s a -> a -> Grammar s ()

  between :: Grammar s () -> Grammar s () -> Grammar s a -> Grammar s a


Grammar - combinators
=====================

.. code:: haskell

  (<<*) :: Grammar s a -> Grammar s () -> Grammar s a

  (*>>) :: Grammar s () -> Grammar s a -> Grammar s a

  replicate :: Natural -> Grammar s a -> Grammar s [a]

  bind
    :: Grammar s a -> (a -> Grammar s b) -> (b -> a)
    -> Grammar s b


Deriving ``Iso``s for custom types
==================================

.. code:: haskell

  {-# LANGUAGE TemplateHaskell #-}

  import Data.Fresnel.TH (makeIso)

  data Foo = A Int Char | B Bool
  makeIso ''Foo

  -- results in splice --

  _Foo :: Iso' (Either (Int, Char) Bool) Foo
  _Foo = ...


.. class:: notes

  - generics-eot
    - not yet worked out how
    - due to "terminator" ``()`` and ``Void``


Putting it all together
=======================

.. code:: haskell

  data PhoneNumber = PhoneNumber
    { areaCode    :: String
    , localNumber :: String
    } deriving (Show)
  makeIso ''PhoneNumber

  phoneNumber :: Cons s s Char Char => Grammar s PhoneNumber
  phoneNumber = _PhoneNumber <<$>>
    between (literal '(') (literal ')') (replicate 2 digit)
    <<*   match (many space) " "
    <<*>> replicate 8 (match (many space) "" *>> digit)


Putting it all together
=======================

.. code:: haskell

  ghci> parse phoneNumber ("(07)3456  78  9  0" :: String)
  Just (PhoneNumber "07" "34567890")

  ghci> print phoneNumber (PhoneNumber "07" "34567890") :: String
  "(07) 34567890"


**************
ASN.1 grammars
**************

Primitive types
===============

.. code:: haskell

  boolean     :: (Cons s s ASN1 ASN1) => Grammar s Bool

  integer     :: (Cons s s ASN1 ASN1) => Grammar s Integer

  octetString :: (Cons s s ASN1 ASN1) => Grammar s B.ByteString

  oid         :: (Cons s s ASN1 ASN1) => Grammar s OID



``OPTIONAL``, ``DEFAULT`` and ``SEQUENCE``
==========================================

.. code:: haskell

  opt :: Grammar s a -> Grammar s (Maybe a)

  def :: (Eq a) => a -> Grammar s a -> Grammar s a

  sequence :: (Cons s s ASN1 ASN1) => Grammar s a -> Grammar s a


ASN.1 grammar - example
=======================

.. code:: haskell

  data BasicConstraints = NotCA | CA (Maybe Natural)

  basicConstraints :: (Cons s s ASN1 ASN1) => Grammar s BasicConstraints
  basicConstraints = _BasicConstraints <<$>> sequence
    (     def False boolean
    <<*>> opt (natural <<$>> integer)  )

  _BasicConstraints :: Iso' (Bool, Maybe Natural) BasicConstraints
  _BasicConstraints = iso f g  where
    f (b, x)  = if b then CA x else NotCA
    g (CA x)  = (True, x)
    g _       = (False, Nothing)


..

  ASN.1 grammar - example
  =======================

  .. code:: haskell

    data AdvReqBody
      = AdvReqBodyGroups (Set OID)
      | AdvReqBodyKey TangKey
    makeIso ''AdvReqBody

    advReqBodyG :: (Cons s s ASN1 ASN1) => Grammar s AdvReqBody
    advReqBodyG = _AdvReqBody <<$>>
      (     tag 0 (setOf oid)
      <<+>> tag 1 tangKeyG        )


***********
Wrapping up
***********

*******************
How did I get here?
*******************

``s/standards/libraries/g``
===========================

.. raw:: latex

  \centering
  \footnotesize
  \setlength{\parskip}{.5em}

.. image:: standards.png
   :width: 75%

CC-BY-NC 2.5 https://xkcd.com/927/


How did I get here?
===================

  1. Evaluate existing solutions
  #. Write :haskell:`Cons`-based parser (to parse :haskell:`[ASN1]`)
  #. Hmm... :haskell:`_Cons` is a prism...
  #. Type tetris
  #. *fresnel* is born


*fresnel* and *fresnel-asn1*
============================

- Repos
  - https://github.com/frasertweedale/hs-fresnel
  - https://github.com/frasertweedale/hs-fresnel-asn1

- AGPLv3+

- I *have* actually used it!


Limitations and caveats
=======================

  - Error reporting

  - Performance

  - I have no idea what I'm doing


What's next?
============

- Publish on Hackage

- Kerberos, X.509, other DER-py applications

- Implement binary DER decoding?


Resources and related topics
============================

- Invertible Syntax Descriptions
  - Paper: `www.informatik.uni-marburg.de/~rendel/unparse/ <http://www.informatik.uni-marburg.de/~rendel/unparse/>`_
  - Libraries: *boomerang*, *roundtrip*, *invertible-syntax*
  - Christian's YLJ2015 talk

- *lens*


Fin
===

.. raw:: latex

  \begin{columns}

    \begin{column}{.4\textwidth}
      \centering
      \includegraphics{Fresnel_lighthouse_lens_diagram.png}
    \end{column}

    \begin{column}{.6\textwidth}

      \setlength{\parskip}{.5em}

      { \centering

      \input{cc-by-ARTIFACT.pdf_tex}

      \copyright~2016  Fraser Tweedale

      { \scriptsize
      Except where otherwise noted this work is licensed under
      }
      { \footnotesize
      \textbf{http://creativecommons.org/licenses/by/4.0/}
      }

      }

      \begin{description}
      \item[Slides]
      \url{https://github.com/frasertweedale/talks/}
      \item[Email]
      \texttt{frase@frase.id.au}
      \item[Twitter]
      \texttt{@hackuador}
      \end{description}
    \end{column}

  \end{columns}
