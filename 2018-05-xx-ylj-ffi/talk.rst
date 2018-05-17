..
  Haskell has a powerful *foreign function interface (FFI)* for
  interfacing with C libraries.  Haskell is a great language for
  building libraries and tools, but interoperability requirements or
  time constraints can make the FFI a compelling option.

  Binding to a non-trivial C library presents several challenges
  including C idioms, memory management, error handling and more.
  This presentation will address a selection of these concerns, using
  *hs-notmuch*[1], a binding to the *notmuch* mail indexer, as a case
  study.  We will discuss:

  - FFI basics and tools to assist binding authors

  - working with "double pointer"-style constructors

  - working with iterators; how to do lazy iteration

  - how to use Haskell's garbage collector to manage lifecycles of
    external objects, and "gotchas" encountered

  - using types to enforce correct use of unsafe APIs

  - performance considerations (including profiling results)

  The presentation will conclude with a mention of some important FFI
  concepts that were not covered (e.g. callbacks) and a look at how
  *hs-notmuch* is being used in the Real World.

  Developers familiar with C will get the most out of this talk
  (because there will be limited time to explain C idioms, memory
  management, etc).  To varying degrees, most of the concepts and
  techniques discussed will apply to other languages' FFIs.

  [1] https://github.com/purebred-mua/hs-notmuch

..

    For main outline / structure, see abstract. Expanding upon each
    of the main aspects listed in the abstract:

    FFI basics and tools to assist binding authors: brief examples
    of simple FFI declarations; brief overview of c2hs and hsc2hs
    tools.

    working with "double pointer"-style constructors: core technique
    shown here (link); example will be simplified (made more
    concrete)

    working with iterators; how to do lazy iteration: core technique
    shown here (link); example will be simplified

    how to use Haskell's garbage collector to manage lifecycles of
    external objects, and "gotchas" encountered: use of GHC
    ForeignPtr to tell Haskell GC to call destructors on foreign
    objects; dealing with talloc (a heirarchical allocator);
    avoiding resource exhaustion (file descriptors owned by live
    foreign objects)

    using types to enforce correct use of unsafe APIs: use of
    phantom types to enforce read-only/read-write database safety;
    phantom types and type-level nats to ensure correct use of
    transaction/locking APIs

    performance considerations (including profiling results):
    performance impact of "safe"/"unsafe" annotation; string
    interning


FFI basics
==========

Why FFI?
--------

- want to do $THING in Haskell
- there exists a C library for $THING
- interoperability / bug-compatibility
- performance / timing-critical code


C FFI
-----

.. code:: haskell

  {-# LANGUAGE ForeignFunctionInterface #-}

  import Foreign.C.Types

  foreign import ccall "math.h sin"
    c_sin :: CDouble -> CDouble

  main :: IO ()
  main = print $ c_sin 1.0


C FFI - linking
---------------

**CLI**::

  ghc Foo.hs -llibname

**Cabal**::

  extra-libraries:
    libname

hsc2hs
------

- part of GHC distribution
- file extension: ``.hsc``
- best support for marshalling structs


c2hs
----

- file extension: ``.chs``
- more features than ``hsc2hs``
- automatic generation of ``foreign import`` declarations

.. code:: cabal

  library
    ...
    build-tools:
      c2hs >= 0.19.1
    ...


``Foreign.Ptr``
---------------

.. code:: haskell

  data Ptr a      -- constructors hidden

  nullPtr :: Ptr a

  plusPtr :: Ptr a -> Int -> Ptr b

  castPtr :: Ptr a -> Ptr b


``Foreign.ForeignPtr``
----------------------

.. code:: haskell

  data ForeignPtr a

  type FinalizerPtr a = FunPtr (Ptr a -> IO ())

  newForeignPtr :: FinalizerPtr a -> Ptr a -> IO (ForeignPtr a)

  withForeignPtr :: ForeignPtr a -> (Ptr a -> IO b) -> IO b



``Foreign.C.String``
--------------------

.. code:: haskell

  type CString = Ptr CChar

  peekCString :: CString -> IO String

  withCString :: String -> (CString -> IO a) -> IO a


``Data.ByteString``
-------------------

.. code:: haskell

  data ByteString

  packCString :: CString -> IO ByteString

  useAsCString :: ByteString -> (CString -> IO a) -> IO a


``Storable``
------------

.. code:: haskell

  class Storable a where
    peek :: Ptr a -> IO a
    ...

  instance Storable (Ptr a)

  alloca :: Storable a => (Ptr a -> IO b) -> IO b


C constructions and idioms
==========================

enum types
----------

.. code:: c

  typedef enum _notmuch_status {
    NOTMUCH_STATUS_SUCCESS = 0,
    NOTMUCH_STATUS_OUT_OF_MEMORY,
    NOTMUCH_STATUS_READ_ONLY_DATABASE,
    NOTMUCH_STATUS_UNBALANCED_FREEZE_THAW,
    ...
  } notmuch_status_t

enum types
----------

.. code:: haskell

  {#enum notmuch_status_t as Status {underscoreToCase} deriving (Eq) #}

enum types
----------

.. code:: haskell

  data Status = StatusSuccess
              | StatusOutOfMemory
              | StatusReadOnlyDatabase
              | StatusUnbalancedFreezeThaw
              ...
    deriving (Eq)

  instance Enum Status where
    ...


opaque pointer types
--------------------

.. code:: c

  typedef struct _notmuch_database notmuch_database_t;

opaque pointer types
--------------------

.. code:: haskell

  {#pointer *notmuch_database_t as DatabaseHandle foreign newtype #}

opaque pointer types
--------------------

.. code:: haskell

  newtype DatabaseHandle = DatabaseHandle (ForeignPtr DatabaseHandle)

  withDatabaseHandle
    :: DatabaseHandle -> (Ptr DatabaseHandle -> IO b) -> IO b
  withDatabaseHandle (DatabaseHandle fptr) =
    withForeignPtr fptr



double-pointer constructors
---------------------------

.. code:: c

  notmuch_status_t
  notmuch_database_open (const char *path,
                         notmuch_database_mode_t mode,
                         notmuch_database_t **database);

double-pointer constructors
---------------------------

.. code:: haskell

  .. {#call notmuch_database_open #} ..

  foreign import ccall "Notmuch/Binding.chs.h notmuch_database_open"
    notmuch_database_open
      :: CString -> CInt -> Ptr (Ptr Database) -> IO CInt


double-pointer constructors
---------------------------

.. code:: haskell

  databaseOpen :: CString -> IO (Either Status DatabaseHandle)
  databaseOpen path =
    let
      mode = fromIntegral (fromEnum DatabaseModeReadWrite)
    in
      alloca $ \ptr -> do
        result <- {#call notmuch_database_open #} path mode ptr
        case toEnum (fromIntegral result) of
          StatusSuccess ->
            Right . DatabaseHandle <$> (peek ptr >>= newForeignPtr_)
          e ->
            pure (Left e)


c-style iterator
----------------

.. code:: c

  notmuch_tags_t *
  notmuch_message_get_tags (notmuch_message_t *message);

  notmuch_bool_t
  notmuch_tags_valid (notmuch_tags_t *tags);

  const char *
  notmuch_tags_get (notmuch_tags_t *tags);

  void
  notmuch_tags_move_to_next (notmuch_tags_t *tags);


c-style iterator - public types
-------------------------------

.. code:: haskell

  data Tag

  tags :: Message n a -> IO [Tag]


c-style iterator - internal types
---------------------------------

.. code:: haskell

  newtype Tag = Tag ByteString

  {#pointer *notmuch_tags_t as Tags newtype #}

  tagFromCString :: CString -> IO Tag

  tagUseAsCString :: Tag -> (CString -> IO a) -> IO a

c-style iterator - internal types
---------------------------------

.. code:: haskell

  newtype Tag = Tag ByteString

  newtype Tags = Tags (Ptr Tags)

  tagFromCString :: CString -> IO Tag

  tagUseAsCString :: Tag -> (CString -> IO a) -> IO a

c-style iterator
----------------

.. code:: haskell

  tagsToList :: Tags -> IO [Tag]
  tagsToList (Tags ptr) = go
    where
    go ptr = test ptr >>= \valid -> case valid of
      0 -> pure []
      _ -> (:)
            <$> (get ptr >>= mk >>= \x -> next ptr $> x)
            <*> (go ptr)

    test = {#call notmuch_tags_valid #}
    get = {#call notmuch_tags_get #}
    next = {#call notmuch_tags_move_to_next #}
    mk = tagFromCString


c macros
--------

.. code:: c

  void *talloc_steal(const void *new_ctx, const void *ptr);

c macros
--------

.. code:: c

  #if (__GNUC__ >= 3)
  #define _TALLOC_TYPEOF(ptr) __typeof__(ptr)
  #define talloc_steal(ctx, ptr) ({ \
    _TALLOC_TYPEOF(ptr) __talloc_steal_ret = (_TALLOC_TYPEOF(ptr)) \
      _talloc_steal_loc((ctx), (ptr), __location__); \
    __talloc_steal_ret; })
  #else /* __GNUC__ >= 3 */
  #define _TALLOC_TYPEOF(ptr) void *
  #define talloc_steal(ctx, ptr) \
    (_TALLOC_TYPEOF(ptr)) _talloc_steal_loc((ctx), (ptr), __location__)
  #endif /* __GNUC__ >= 3 */
  void *_talloc_steal_loc(
    const void *new_ctx, const void *ptr, const char *location);

c macros
--------

**TODO**  this is fine

c macros
--------

Two options:

- bind to non-public API (e.g. ``_talloc_steal_loc``)
- write *c bits*


external object lifecycles
--------------------------

.. code:: c

  notmuch_query_t *
  notmuch_query_create (notmuch_database_t *database,
                        const char *query_string);

  void
  notmuch_query_destroy (notmuch_query_t *query);

external object lifecycles
--------------------------

.. code:: haskell

  foreign import ccall "&notmuch_query_destroy"
    query_destroy :: FinalizerPtr a

external object lifecycles
--------------------------

.. code:: haskell

  query_create :: DatabaseHandle -> String -> IO (Query a)
  query_create db s = withCString s $ \s' ->
    withDatabaseHandle db $ \db' ->
      {#call notmuch_query_create #} db' s'

        >>= fmap QueryHandle . newForeignPtr query_destroy

external object lifecycles
--------------------------

.. code:: haskell

  query_create :: DatabaseHandle -> String -> IO (Query a)
  query_create db s = withCString s $ \s' ->
    withDatabaseHandle db $ \db' ->
      {#call notmuch_query_create #} db' s'
        >>= detachPtr
        >>= fmap QueryHandle . newForeignPtr query_destroy

external object lifecycles
--------------------------

.. code:: haskell

  detachPtr :: Ptr a -> IO (Ptr a)
  detachPtr ptr = castPtr <$>
    {#call _talloc_steal_loc #}
      nullPtr           -- new context (no parent)
      (castPtr ptr)
      nullPtr


API safety - read-only mode
---------------------------

.. code:: c

  notmuch_status_t
  notmuch_message_add_tag (notmuch_message_t *message, const char *tag);

API safety - read-only mode
---------------------------

.. code:: haskell

  {#enum database_mode_t as DatabaseMode {underscoreToCase} #}

API safety - read-only mode
---------------------------

.. code:: haskell

  data DatabaseMode = DatabaseModeReadOnly
                    | DatabaseModeReadWrite

  instance Enum DatabaseMode where
    ...


API safety - read-only mode
---------------------------

.. code:: haskell

  {-# LANGUAGE DataKinds #-}

  newtype Database (a :: DatabaseMode) = Database DatabaseHandle

  withDatabase :: Database a -> (Ptr DatabaseHandle -> IO b) -> IO b
  withDatabase (Database dbh) = withDatabaseHandle dbh

  data Message (a :: DatabaseMode) = Message MessageHandle


API safety - read-only mode
---------------------------

.. code:: haskell

  class Mode a where
    getMode :: Proxy a -> DatabaseMode

  instance Mode 'DatabaseModeReadOnly where
    getMode _ = DatabaseModeReadOnly

  instance Mode 'DatabaseModeReadWrite where
    getMode _ = DatabaseModeReadWrite


API safety - read-only mode
---------------------------

.. code:: haskell

  {-# LANGUAGE ScopedTypeVariables #-}

  databaseOpen
    :: forall a. Mode a
    => CString -> IO (Either Status (Database a))
  databaseOpen path =
    let
      mode = getMode (Proxy :: Proxy a)
    in
      ...

API safety - read-only mode
---------------------------

.. code:: haskell

  messageAddTag :: Message 'DatabaseModeReadWrite -> Tag -> IO ()
  messageAddTag msg tag = void $ withMessage msg $
    tagUseAsCString tag . {#call notmuch_message_add_tag #}


API safety - locking
--------------------

.. code:: c

  notmuch_status_t
  notmuch_message_freeze (notmuch_message_t *message);

  /* can return NOTMUCH_STATUS_READ_ONLY_DATABASE
     or NOTMUCH_STATUS_UNBALANCED_FREEZE_THAW      */
  notmuch_status_t
  notmuch_message_thaw (notmuch_message_t *message);

API safety - locking
--------------------

.. code:: haskell

  {-# LANGUAGE DataKinds #-}
  {-# LANGUAGE TypeFamilies #-}
  {-# LANGUAGE TypeOperators #-}

  import GHC.TypeLits

  data Message (n :: Nat) (a :: DatabaseMode) = Message MessageHandle

  messageAddTag :: Message n 'DatabaseModeReadWrite -> Tag -> IO ()
  messageAddTag msg tag = void $ withMessage msg $
    tagUseAsCString tag . {#call notmuch_message_add_tag #}

API safety - locking
--------------------

.. code:: haskell

  messageFreeze :: Message n RW -> IO (Message (n + 1) RW)
  messageFreeze msg =
    withMessage msg {#call notmuch_message_freeze #} $> coerce msg

  messageThaw :: (1 <= n) => Message n RW -> IO (Message (n - 1) RW)
  message_thaw msg =
    withMessage msg {#call notmuch_message_thaw #} $> coerce msg

  withFrozenMessage :: (Message 1 RW -> IO a) -> Message 0 RW -> IO a
  withFrozenMessage k msg = bracket (message_freeze msg) message_thaw k

.. NOTE: bring on linear types!

Performance
===========

``unsafe``
----------

.. code:: haskell

  foreign import ccall        "notmuch.h notmuch_messages_valid"
    notmuch_messages_valid :: Messages -> IO CInt

  foreign import ccall        "notmuch.h notmuch_messages_get"
    notmuch_messages_get :: Messages -> IO CString

  foreign import ccall        "notmuch.h notmuch_messages_move_to_next"
    notmuch_messages_move_to_next :: Messages -> IO ()

``unsafe``
----------

.. code:: haskell

  foreign import ccall unsafe "notmuch.h notmuch_messages_valid"
    notmuch_messages_valid :: Messages -> IO CInt

  foreign import ccall unsafe "notmuch.h notmuch_messages_get"
    notmuch_messages_get :: Messages -> IO CString

  foreign import ccall unsafe "notmuch.h notmuch_messages_move_to_next"
    notmuch_messages_move_to_next :: Messages -> IO ()

``unsafe``
----------

.. code:: haskell

  {#call        notmuch_messages_valid #}

  {#call        notmuch_messages_get #}

  {#call        notmuch_messages_move_to_next #}

``unsafe``
----------

.. code:: haskell

  {#call unsafe notmuch_messages_valid #}

  {#call unsafe notmuch_messages_get #}

  {#call unsafe notmuch_messages_move_to_next #}

``unsafe``
----------

Before::

  total time  =        6.53 secs   (6530 ticks @ 1000 us, 1 processor)
  total alloc = 260,249,536 bytes  (excludes profiling overheads)

After::

  total time  =        3.73 secs   (3728 ticks @ 1000 us, 1 processor)
  total alloc = 260,249,536 bytes  (excludes profiling overheads)


lazy iteration
--------------

.. code:: haskell

   
   
  messagesToList :: Messages -> IO [Message n a]
  messagesToList (Messages ptr) = go
    where
    go ptr = test ptr >>= \valid -> case valid of
      0 -> pure []
      _ -> (:)
            <$> (get ptr >>= mk >>= \x -> next ptr $> x)
            <*> (go ptr)

lazy iteration
--------------

.. code:: haskell

  import System.IO.Unsafe (unsafeInterleaveIO)

  messagesToList :: Messages -> IO [Message n a]
  messagesToList (Messages ptr) = go
    where
    go ptr = test ptr >>= \valid -> case valid of
      0 -> pure []
      _ -> (:)
            <$> (get ptr >>= mk >>= \x -> next ptr $> x)
            <*> unsafeInterleaveIO (go ptr)

lazy iteration (search ``*``, take 10, count tags)
--------------------------------------------------

Before::

    total time  =        1.79 secs   (1795 ticks @ 1000 us, 1 processor)
    total alloc =  59,500,568 bytes  (excludes profiling overheads)

After::

    total time  =        0.07 secs   (68 ticks @ 1000 us, 1 processor)
    total alloc =      79,960 bytes  (excludes profiling overheads)

lazy iteration (search ``*``, count tags)
-----------------------------------------

Before::

  68,431,240 bytes maximum residency (9 sample(s))

  total time  =        8.37 secs   (8370 ticks @ 1000 us, 1 processor)
  total alloc = 218,627,008 bytes  (excludes profiling overheads)

After::

  40,965,384 bytes maximum residency (8 sample(s))

  total time  =        7.59 secs   (7586 ticks @ 1000 us, 1 processor)
  total alloc = 257,666,440 bytes  (excludes profiling overheads)


avoiding string copies
----------------------

.. code:: haskell

  data Tag = Tag ByteString

  tagFromCString :: CString -> IO Tag
  tagFromCString ptr = Tag <$> do
    n <- c_strlen ptr
    packCStringLen (ptr, fromIntegral n + 1)

  tagUseAsCString :: Tag -> (CString -> IO a) -> IO a
  tagUseAsCString (Tag bs) =
    unsafeUseAsCString bs

avoiding string copies
----------------------

.. code:: haskell

  data Tag = Tag (ForeignPtr CChar)

  tagFromCString :: CString -> IO Tag
  tagFromCString =
    detachPtr >=> fmap Tag . newForeignPtr talloc_free


  tagUseAsCString :: Tag -> (CString -> IO a) -> IO a
  tagUseAsCString (Tag bs) =
    unsafeUseAsCString bs

avoiding string copies - interning
----------------------------------

- keep exactly one copy of each string
- zero-copy ``unsafeUseAsCString`` if null-terminated
- *O(1)* equality
- *intern* package by Ed Kmett


Things that weren't covered
---------------------------

- foreign export
- marshalling C structs (*hsc2hs*)
- other FFIs (JVM, JavaScript, ...)
