-- Haskell type classes cf Java interfaces
--
-- * similar to interfaces (but not really)
-- * Java implementations are declared within the class
-- * Haskell instances are declared separately from the type
-- * type classes are more general
--   * multi-parameter type classes
--   * no implicit "invocant" argument (e.g. Comparable.compareTo)
-- * in Java, two arguments implementing a certain interface
--   are not guaranteed to have same type

{-# LANGUAGE MultiParamTypeClasses #-}

class Foo a b where
  foo :: a -> b -> Bool

(>) :: Ord a => a -> a -> Bool
(>) = undefined
