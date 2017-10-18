module Gotcha where

import Test.QuickCheck

----------------
-- EXHAUSTION --
----------------

prop_EvenHalveThenDouble :: [Int] -> Property
prop_EvenHalveThenDouble xs =
  all even xs ==>
    map ((*2) . (`div` 2)) xs == xs

newtype EvenList a = EvenList [a]
  deriving (Show)

instance (Arbitrary a, Num a) => Arbitrary (EvenList a) where
  arbitrary = (EvenList . fmap (*2)) <$> oneof
    [ pure []
    , (:) <$> arbitrary <*> arbitrary
    ]

prop_EvenListHalveThenDouble :: EvenList Int -> Bool
prop_EvenListHalveThenDouble (EvenList xs) =
  map ((*2) . (`div` 2)) xs == xs


------------------
-- TRIVIAL DATA --
------------------

prop_EvenListAllEven :: EvenList Int -> Property
prop_EvenListAllEven (EvenList xs) =
  collect (length xs) $
    all even xs

prop_EvenListAllEven' :: EvenList Int -> Property
prop_EvenListAllEven' (EvenList xs) =
  cover (length xs > 1) 75 "non-empty" $
    all even xs


-------------------
-- INFINITE DATA --
-------------------

data Tree a
  = Leaf
  | Node (Tree a) a (Tree a)
  deriving (Show)

count :: Tree a -> Int
count Leaf = 0
count (Node l _ r) = count l + count r + 1

-- DANGEROUS
{-
instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = frequency
    [ (1, pure Leaf)
    , (3, Node <$> arbitrary <*> arbitrary <*> arbitrary)
    ]
-}

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = sized gen where
    gen 0 = pure Leaf
    gen n = let n' = n `div` 2 in frequency
      [ (1, pure Leaf)
      , (3, Node <$> gen n' <*> arbitrary <*> gen n')
      ]

prop_TreeCount :: Tree Int -> Bool
prop_TreeCount t = count t >= 0
