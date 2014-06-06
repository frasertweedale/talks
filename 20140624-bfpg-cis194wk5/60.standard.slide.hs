-- Standard type classes: Ord
--
-- For types with *total order*

{-# LANGUAGE NoImplicitPrelude #-}
import Prelude (Bool, Eq)

class Eq a => Ord a where
  compare :: a -> a -> Ordering
  (<)     :: a -> a -> Bool
  (>=)    :: a -> a -> Bool
  (>)     :: a -> a -> Bool
  (<=)    :: a -> a -> Bool
  min     :: a -> a -> a
  max     :: a -> a -> a

data Ordering = LT | EQ | GT
