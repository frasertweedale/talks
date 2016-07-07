import Data.Foldable (toList)
import Data.List (sort)

import Control.DeepSeq (force)
import Criterion.Main
import Data.Set (Set, fromList)

main :: IO ()
main =
  let
    n = 10000
    s :: Set Integer
    s = fromList [n,n-1..1]
  in
    defaultMain
      [ bench "Data.Set" $ nf (take 5 . toSortedList) (force s)
      ]

toSortedList :: (Foldable t, Ord a) => t a -> [a]
toSortedList = sort . toList
