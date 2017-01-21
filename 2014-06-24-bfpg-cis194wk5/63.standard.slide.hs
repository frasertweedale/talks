-- Standard type classes: Integral
--

class (Real a, Enum a) => Integral a where
  quot      :: a -> a -> a
  rem       :: a -> a -> a
  div       :: a -> a -> a
  mod       :: a -> a -> a
  quotRem   :: a -> a -> (a, a)
  divMod    :: a -> a -> (a, a)
  toInteger :: a -> Integer

-- instances for Integer and Int
