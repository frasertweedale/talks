..
  Copyright 2015  Fraser Tweedale.

  This work is licensed under the Creative Commons Attribution 4.0
  International License. To view a copy of this license, visit
  http://creativecommons.org/licenses/by/4.0/.


*****
Intro
*****


This talk
=========

- What am FFI?

- Haskell C FFI

- ``c2hs``

- Other tools and resources


What is an FFI?
===============

- Mechanism to call code written in one langauge
  from code written in another language

- Often used to *bind* or *wrap* an entire library (usually C)

- Sometimes used to *export* routines to be called from other
  language


C - important aspects
=====================

- Header files (``*.h``)

- *Linking* to library object code

- Pointers
  - *Oh my God, it's full of stars!*


***********
Haskell FFI
***********


Haskell FFI
===========

.. code:: haskell

  {-# LANGUAGE ForeignFunctionInterface #-}

  import Foreign
  import Foreign.C.Types

  foreign import ccall unsafe "math.h sin"
    c_sin :: CDouble -> CDouble

  foreign import ccall unsafe "stdlib.h rand"
    c_rand :: IO CUInt


Haskell FFI
===========

- *Your* responsibility to use ``IO`` where appropriate

- ``safe`` vs ``unsafe``


Haskell FFI - pointers
======================

.. code:: haskell

  #include <thing.h>

  foreign import ccall "thing_get_version"
    thing_get_version :: Ptr Thing -> IO CUInt

  foreign import ccall "thing_get_name"
    thing_get_name :: Ptr Thing -> IO (Ptr CChar)

  foreign import ccall "thing_new"
    thing_new :: Ptr CChar -> Ptr (Ptr Thing) -> IO CUInt


Haskell FFI - pointers
======================

.. code:: haskell

  withCString :: String -> (CString -> IO a) -> IO a
  peekCString :: CString -> IO String

  peek :: Ptr a -> IO a
  alloca :: Storable a => (Ptr a -> IO b) -> IO b



Haskell FFI - pointers
======================

.. code:: haskell

  newtype Thing = Thing (Ptr Thing)

  makeThing :: String -> IO (Maybe Thing)
  makeThing s =
    withCString s $ \name ->
      alloca $ \ptr ->
        result <- thing_new name ptr
        if result == 0
          then Just . Thing <$> peek ptr
          else return Nothing


Haskell FFI - garbage collection
================================

.. code:: haskell

  foreign import ccall "&thing_free"
    thing_free :: FinalizerPtr a

  newtype Thing = Thing (FinalizerPtr Thing)

  makeThing :: String -> IO (Maybe Thing)
  makeThing s =
    withCString s $ \name ->
      alloca $ \ptr ->
        result <- thing_new name ptr
        if result == 0
          then Just . Thing . newForeignPtr thing_free
            <$> peek ptr
          else return Nothing


********
``c2hs``
********

``c2hs``
========

- Preprocessor to simplify bindings

- Enums and typedefs

- Automatic ``foreign import`` (demand-based)

- ``*.chs`` file extensions; outputs ``*.hs``


Detour: ``notmuch``
===================

- Mail indexer

- Written in C++ (exports C interface)

- I have written a binding (immature)


``c2hs`` - enum (C)
===================

.. code:: c

  typedef enum {
      NOTMUCH_SORT_OLDEST_FIRST,
      NOTMUCH_SORT_NEWEST_FIRST,
      NOTMUCH_SORT_MESSAGE_ID,
      NOTMUCH_SORT_UNSORTED
  } notmuch_sort_t;


``c2hs`` - enum (Haskell)
=========================

.. code:: haskell

  {#enum notmuch_sort_t as Sort {underscoreToCase} #}

  -- TURNS INTO --

  data Sort = SortOldestFirst
            | SortNewestFirst
            | SortMessageId
            | SortUnsorted
            deriving (Enum)


``c2hs`` - typedef
==================

.. code:: c

  /* C */
  typedef struct _notmuch_message notmuch_message_t;

.. code:: haskell

  -- HASKELL --
  {#pointer *notmuch_message_t as Message foreign newtype #}

  -- TURNS INTO --

  newtype Message = Message (ForeignPtr (Message))
  withMessage :: Message -> (Ptr Message -> IO b) -> IO b
  withMessage (Message fptr) = withForeignPtr fptr


``c2hs`` - calls (C)
====================

.. code:: c

  const char *
  notmuch_message_get_message_id(
    notmuch_message_t *message
    );


``c2hs`` - calls (Haskell)
==========================

.. code:: haskell

  message_get_message_id :: Message -> IO String
  message_get_message_id ptr =
    withMessage ptr
      ( {#call notmuch_message_get_message_id #}
        >=> peekCString )

  -- TURNS INTO --

  message_get_message_id :: Message -> IO String
  message_get_message_id ptr =
    withMessage ptr
      (notmuch_message_get_message_id >=> peekCString)

  foreign import ccall safe "notmuch_message_get_message_id"
    notmuch_message_get_message_id
      :: ((Ptr (Message)) -> (IO (Ptr CChar)))


``c2hs``
========


- ``{#context prefix = "notmuch" #}``

- Finalisers must be manually attached (``addForeignPtrFinalizer``)

- ``pointer`` directive without ``newtype`` makes type synonym


***********
Other tools
***********

``cabal``
=========

::

  build-tools:
    c2hs >= 0.15

  extra-libraries:
    notmuch


``hsc2hs``
==========

- ``*.hsc`` (compare ``*.chs``)

- Can bind to ``#define``, e.g. macros, constants

- Better ``Storable`` instance automation

- Discussion of differences (Stack Overflow): http://is.gd/weyuYN


More tools
==========

- ``c2hsc``
  - ``.h`` -> ``.hcs``, ``.hsc.helper.c``

- ``bindings-DSL``
  - CPP macros to help write bindings


**********
Conclusion
**********

What we covered
===============

- FFI - what and why?

- Direct C FFI usage

- ``c2hs`` examples

- Role call of other tools

- You can write a binding now!


What we didn't cover
====================

- Calling Haskell from C (``foreign export``)

- Dealing with external GCs, "special" allocators
  - I have war stories

- ``Storable`` typeclass

- ``hsc2hs`` or other tools in any detail

- Other Haskell FFIs (e.g. ``JavaScriptFFI``)


Resources
=========

- Haskell 98 FFI Report: http://is.gd/ObI8Gn
- https://en.wikibooks.org/wiki/Haskell/FFI
- *Real World Haskell* chapter: http://is.gd/Rov6w5
- ``c2hs`` documentation: http://is.gd/JpX0Ku
- https://github.com/frasertweedale/hs-notmuch


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
