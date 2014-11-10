module Expr where

import Control.Applicative
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
elimMul1 (Lit x)         = Lit x
elimMul1 (Add a b)       = Add (elimMul1 a) (elimMul1 b)
elimMul1 (Mul a (Lit 1)) = elimMul1 a
elimMul1 (Mul (Lit 1) a) = elimMul1 a
elimMul1 (Mul a b)       = Mul (elimMul1 a) (elimMul1 b)

instance Arbitrary Expr where
  arbitrary = sized gen
    where
    gen 0 = Lit <$> oneof [pure 0, pure 1, arbitrary]
    gen n = let n' = n `div` 2 in oneof
      [ Lit <$> arbitrary
      , Add <$> gen n' <*> gen n'
      , Mul <$> gen n' <*> gen n'
      ]

prop_elimMul1Equiv :: Expr -> Property
prop_elimMul1Equiv e = property $ eval e == eval (elimMul1 e)

prop_elimMul1Elim :: Expr -> Property
prop_elimMul1Elim expr = property $ f (elimMul1 expr) where
  f (Mul _ (Lit 1)) = False
  f (Mul (Lit 1) _) = False
  f (Mul a b) = f a && f b
  f (Add a b) = f a && f b
  f (Lit _) = True
