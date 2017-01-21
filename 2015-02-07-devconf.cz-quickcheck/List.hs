module List where

import Test.QuickCheck

rev :: [a] -> [a]
rev [] = []
rev (x:xs) = rev xs ++ [x]

prop_RevUnit :: Int -> Bool
prop_RevUnit x =
  rev [x] == [x]

prop_RevApp :: [Int] -> [Int] -> Bool
prop_RevApp xs ys =
  rev (xs ++ ys) == rev ys ++ rev xs
