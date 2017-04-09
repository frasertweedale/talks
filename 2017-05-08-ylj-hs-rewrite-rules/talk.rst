There's a hole in my program...
===============================

.. role:: haskell(code)
   :language: haskell

.. role:: latex(raw)
   :format: latex

.. code:: haskell

             _ :: B.ByteString -> L.ByteString
             _ :: B.ByteString -> [Word8]
             _ :: T.Text -> String
             _ :: String -> T.Text
             _ :: TL.Text -> T.Text


There's a hole in my program...
===============================

.. code:: haskell

  L.fromStrict :: B.ByteString -> L.ByteString
  B.unpack     :: B.ByteString -> [Word8]
  T.unpack     :: T.Text -> String
  T.pack       :: String -> T.Text
  TL.toStrict  :: TL.Text -> T.Text


:haskell:`Control.Lens.Cons`
============================

.. code:: haskell

  class Cons s t a b where
    _Cons :: Prism s t (a, s) (b, t)

  instance ByteString ByteString Word8 Word8
  instance Text Text Char Char
  instance [a] [b] a b

  cons   :: Cons s s a a => a -> s -> s
  uncons :: Cons s s a a => s -> Maybe (a, s)


``recons``
==========

.. code:: haskell

  recons
    :: (Cons s1 s1 a a, Cons s2 s2 a a, AsEmpty s2)
    => s1 -> s2


There's a hole in my program...
===============================

.. code:: haskell

             _ :: B.ByteString -> L.ByteString
             _ :: B.ByteString -> [Word8]
             _ :: T.Text -> String
             _ :: String -> T.Text
             _ :: TL.Text -> T.Text


There's a hole in my program...
===============================

.. code:: haskell

        recons :: B.ByteString -> L.ByteString
        recons :: B.ByteString -> [Word8]
        recons :: T.Text -> String
        recons :: String -> T.Text
        recons :: TL.Text -> T.Text



Why not concrete functions?
===========================

- Reuse
- Refactoring
- Readability (**parametricity**)


Parametricity
=============

.. code:: haskell

  mibbup
    :: (Cons s1 s1 a a, Cons s2 s2 a a, AsEmpty s2)
    => s1 -> s2

  wossit
    :: [Word8] -> L.ByteString


Parametricity
=============

.. code:: haskell

  recons
    :: (Cons s1 s1 a a, Cons s2 s2 a a, AsEmpty s2)
    => s1 -> s2

  L.pack
    :: [Word8] -> L.ByteString


*criterion*
===========

.. code:: haskell

  whnf :: (a -> b) -> a -> Benchmarkable

  nf :: NFData b => (a -> b) -> a -> Benchmarkable

  bench :: String -> Benchmarkable -> Benchmark


Glasgow Haskell Compiler
========================

- Haskell desugars to ***Core***
- ***Simplifier*** applies Core-to-Core transformations
- :latex:`Core $\to$ STG $\to$ \textbf{C{-}{-}}`
- Compile C\-\- to machine code

.. raw:: latex

  \bigskip

  \tiny

  Secrets of the Haskell Inliner:
  \url{https://www.microsoft.com/en-us/research/wp-content/uploads/2016/07/inline-jfp.pdf}

  Compilation by transformation:
  \url{https://www.microsoft.com/en-us/research/wp-content/uploads/1998/09/comp-by-trans-scp.pdf}


Phase control
=============

.. code:: haskell

  --                            Before phase 2     Phase 2 and later
  {-# INLINE   f #-}       --      Yes                Yes
  {-# NOINLINE f #-}       --      No                 No

  {-# INLINE   [2]  f #-}  --      No                 Yes
  {-# INLINE   [~2] f #-}  --      Yes                No
  {-# NOINLINE [2]  f #-}  --      No                 Maybe
  {-# NOINLINE [~2] f #-}  --      Maybe              No


Rewrite rules
=============

.. code:: haskell

  {-# RULES

  "map/map"     forall f g xs.
                map f (map g xs) = map (f . g) xs

  #-}


Rewrite rules
=============

.. code:: haskell

  {-# RULES

  "map/map" [2] forall f g xs.
                map f (map g xs) = map (f . g) xs

  #-}


Rewrite rules
=============

- LHS rewrites to RHS
- always exported
- compile with ``-O``
- no termination / semantic equivalence checks


Rewrite rules
=============

.. code:: haskell

  {-# RULES

  "whups" forall x y.  f x y = f y x

  #-}


Rewrite rules
=============

.. code:: haskell

  {-# RULES

  "rev-involutive" forall xs.
                   reverse (reverse xs) = xs

  #-}

.. note: seq and ⊥


Rewrite rules - *fusion*
========================

.. code:: haskell

  data Stream a where
    Stream :: (s -> Step s a) -> s -> Stream a

  data Step s a = Yield a s | Skip s | Done

  map :: (a -> b) -> [a] -> [b]
  map f = unstream . map' . stream

  -- http://code.haskell.org/~dons/papers/icfp088-coutts.pdf



Rewrite rules - *fusion*
========================

.. code:: haskell

  map g . map f
    = unstream . map' g . stream . unstream . map' f . stream


Rewrite rules - *fusion*
========================

.. code:: haskell

  map g . map f
    = unstream . map' g .        id         . map' f . stream


Rewrite rules - *fusion*
========================

.. code:: haskell

  map g . map f
    = unstream . map' g . map' f . stream


Compiler options
================

::

  -ddump-rule-firings

  -ddump-rule-rewrites

  -ddump-inlinings

  -ddump-simpl-iterations


*concise*
=========

.. code:: haskell

  Control.Lens.Cons.Extras.recons
    :: (Cons s1 s1 a a, Cons s2 s2 a a, AsEmpty s2)
    => Getter s1 s2

  -- https://hackage.haskell.org/package/concise


*fresnel*
=========

.. code:: haskell

  sepBy :: Grammar s a -> Grammar s () -> Grammar s [a]

  sepByT :: Grammar T.Text a -> Grammar T.Text () -> Grammar T.Text [a]
  sepByT g sep = prism'
    (\(as, s) -> T.intercalate (print sep ()) (print g <$> as) <> s)
    (preview (sepBy g sep))

  {-# RULES "sepBy/T" sepBy = sepByT #-}

  -- https://github.com/frasertweedale/hs-fresnel


Recap
=====

- The **three Rs**: reuse, refactoring, readability
- Rewrite rules: make generic functions go fast
- Other optimisations enabled by rewrite rules
- **Phase control**: make inliner and simplifier play nice


.. references:

  Rewrite rules blog post:
  https://www.stackbuilders.com/tutorials/haskell/ghc-optimization-and-fusion/

  Stream fusion:
  http://code.haskell.org/~dons/papers/icfp088-coutts.pdf
  https://www.microsoft.com/en-us/research/wp-content/uploads/2016/07/haskell-beats-C.pdf

  Secrets of the Haskell Inliner:
  https://www.microsoft.com/en-us/research/wp-content/uploads/2016/07/inline-jfp.pdf

  Compilation by transformation:
  https://www.microsoft.com/en-us/research/wp-content/uploads/1998/09/comp-by-trans-scp.pdf

  
==

.. raw:: latex

    \setlength{\parskip}{.5em}

    { \centering

    \input{cc-by-ARTIFACT.pdf_tex}
    \\
    { \scriptsize
    Except where otherwise noted this work is licensed under
    }\\
    { \footnotesize
    \textbf{http://creativecommons.org/licenses/by/4.0/}
    }

    \bigskip
    \Large

    \texttt{\url{https://speakerdeck.com/frasertweedale}}

    \texttt{@hackuador}

    }
