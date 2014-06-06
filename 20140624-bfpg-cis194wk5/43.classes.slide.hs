-- Type classes 4: derivation
--

data Foo = F Int | G Char
  deriving (Eq, Ord, Show)
