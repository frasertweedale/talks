-- Parametricity 3
--
-- NOTES:
--
-- * Haskell types are *erased* at compile time
-- * Lingo:   "parametric"
--          = "works uniformly for any type used by caller"
-- * Kind of like Java generics

-- Possible implementations of f :: a -> a -> a
f1 :: a -> a -> a
f1 x _ = x
                    -- OR --
f2 :: a -> a -> a
f2 _ y = y
