-- Standard type classes: Num
--
-- For numeric types

class Num a where
  (+) :: a -> a -> a
  (*) :: a -> a -> a
  (-) :: a -> a -> a
  negate :: a -> a
  abs :: a -> a
  signum :: a -> a
  fromInteger :: Integer -> a

-- instances include Integer, Int, Float, Double
-- side note: integer literals are polymorphic
