module Expr where

import Test.QuickCheck

data Expr
  = Lit Integer
  | Add Expr Expr
  | Mul Expr Expr
  deriving (Show, Eq)

eval :: Expr -> Integer
eval (Lit x) = x
eval (Add a b) = eval a + eval b
eval (Mul a b) = eval a * eval b

-- |
-- prop> prop_elimMul1Equiv
-- prop> prop_elimMul1Elim
--
elimMul1 :: Expr -> Expr
elimMul1 (Lit x)   = Lit x
elimMul1 (Add a b) = Add (elimMul1 a) (elimMul1 b)
elimMul1 (Mul a b) =
  case (a, b) of
    (Lit 1, y) -> elimMul1 y
    (x, Lit 1) -> elimMul1 x
    (x, y)     -> Mul (elimMul1 x) (elimMul1 y)

prop_elimMul1Elim :: Expr -> Bool
prop_elimMul1Elim expr = f (elimMul1 expr) where
  f (Mul _ (Lit 1)) = False
  f (Mul (Lit 1) _) = False
  f (Mul a b) = f a && f b
  f (Add a b) = f a && f b
  f (Lit _) = True

prop_elimMul1Equiv :: Expr -> Bool
prop_elimMul1Equiv e = eval e == eval (elimMul1 e)

instance Arbitrary Expr where
  arbitrary = sized gen
    where
    gen 0 = Lit <$> oneof [pure 0, pure 1, arbitrary]
    gen n = let n' = n `div` 2 in oneof
      [ Lit <$> arbitrary
      , Add <$> gen n' <*> gen n'
      , Mul <$> gen n' <*> gen n'
      ]
  shrink (Add a b) = [a, b] ++ (Add <$> shrink a <*> shrink b)
  shrink (Mul a b) = [a, b] ++ (Mul <$> shrink a <*> shrink b)
  shrink _ = []
