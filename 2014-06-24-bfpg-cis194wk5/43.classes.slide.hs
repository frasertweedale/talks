-- Type classes 4: instances and derivation
--

data Foo = F Int | G Char     -- deriving (Eq, Ord, Show)

instance Eq Foo where
  F n == F m    =   n == m    -- Int and Char have Eq instances
  G c == G c'   =   c == c'
  _   == _      =   False
