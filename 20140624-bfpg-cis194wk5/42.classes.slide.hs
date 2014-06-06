-- Type classes 3
--
{-# LANGUAGE NoImplicitPrelude #-}
import Prelude (Bool, not)
{-# ANN module "HLint: ignore Use ==" #-}
{-# ANN module "HLint: ignore Use /=" #-}

-- | Actual declaration of Eq
--
class Eq a where
  (==), (/=) :: a -> a -> Bool
  x == y = not (x /= y)
  x /= y = not (x == y)
