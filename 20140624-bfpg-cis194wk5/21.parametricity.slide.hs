-- Parametricity 2
--

f :: a -> a -> a
f a1 a2 = case typeOf a1 of
  Int -> a1 + a2
  Bool -> a1 && a2
  _ -> a1

{- We could write this in many other languages:

def f(a, b):
  if isinstance(a, int):
    return a + b
  elif isinstance(a, bool):
    return a and b
  else
    return a

-}

data Type = Int | Bool | Other
typeOf :: a -> Type
typeOf = const Int
