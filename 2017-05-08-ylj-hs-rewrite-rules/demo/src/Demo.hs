module Demo
  ( recons
  , unfoldr
  ) where

import Control.Lens
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

recons
  :: (Cons s1 s1 a a, Cons s2 s2 a a, AsEmpty s2)
  => s1 -> s2
