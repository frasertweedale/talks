..
  Copyright 2015  Red Hat, Inc.

  This work is licensed under the Creative Commons Attribution 4.0
  International License. To view a copy of this license, visit
  http://creativecommons.org/licenses/by/4.0/.


Confession
==========

- I hardly know anything about parsing

- But I know I don't like repeating myself

- And I know a bit about APIs and abstraction


Parser
======

.. code:: haskell

  newtype Parser a = Parser
    { runParser :: String -> Maybe (a, String) }


Parser API
==========

.. code:: haskell

  data Thing = Thing Int Bool

  -- serialised repr: "555t", "7f"
  -- not valid:       "t555", "7 f"

  -- want to define parser thusly
  parseThing :: Parser Thing
  parseThing = Thing  $  parseInt `magic` parseBool


Parser API
==========

.. code:: haskell

  data Thing = Thing Int Bool

  -- serialised repr: "555t", "7f"
  -- not valid:       "t555", "7 f"

  -- actual API
  parseThing :: Parser Thing
  parseThing = Thing <$> parseInt   <*>   parseBool


Parser API
==========

.. code:: haskell

  (<|>)   :: Parser a -> Parser a -> Parser a

  many    :: Parser a -> Parser [a]

  many1   :: Parser a -> Parser (NonEmpty a)

  sepBy   :: Parser a -> Parser b -> Parser [a]

  between :: Parser l -> Parser r -> Parser a -> Parser a


Let's write some code!
======================


What if...
==========

- Input type is not ``String`` (e.g. ``Text``)?

- *Piecewise* input type is not ``Char`` (e.g. ``Word8``)?

- Should you have to use a separate library / module?


Better Parser API
=================

.. code:: haskell

  data Amino = G | A | T | C
  type DNA = [Amino]

  parseHackerGene :: Parser DNA (NonEmpty Amino)
  parseHackerGene = many1 (symbol A <|> symbol C)


Better Parser API - enter ``Cons``
==================================

.. code:: haskell

  class Cons s t a b | s -> a, t -> b, s b -> t, t a -> s
    where
    _Cons :: Prism s t (a, s) (b, t)

  uncons :: Cons s s a a => s -> Maybe (a, s)

  instance Cons ByteString ByteString Word8 Word8
  instance Cons Text Text Char Char
  instance Cons [a] [b] a b
  instance Cons (Vector a) (Vector b) a b
  -- and many more!


Better Parser API
=================

.. code:: haskell

  import Control.Lens.Cons

  newtype Parser s a = Parser
    { runParser :: s -> Maybe (a, s) }

  satisfy
    :: (Cons s s a a, Eq a)
    => (a -> Bool) -> Parser s a


Let's refactor some code!
=========================


Haskell parsing libs
====================

- Parsec

- Attoparsec

- Trifecta

- uu-parsinglib

- Parsers (unifying interface)


Design considerations
=====================

- Ambiguous parses

- Incremental input

- Errors and recovery

- Performance
  - Backtracking
  - Continuation passing style

- Transformers


Resources and related topics
============================

- Monadic Parsing in Haskell (Functional Pearl)
  - `www.cs.uwyo.edu/~jlc/courses/3015/parser_pearl.pdf <http://www.cs.uwyo.edu/~jlc/courses/3015/parser_pearl.pdf>`_

- Lexer / parser generators (alex / happy)

- Invertible Syntax Descriptions
  - Paper: `www.informatik.uni-marburg.de/~rendel/unparse/ <http://www.informatik.uni-marburg.de/~rendel/unparse/>`_

- Prisms, lenses and other optics

- Zippers
  - George's Argonaut talk next month!


Fin
===

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
