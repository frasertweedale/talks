-- Example: type class constraint on functions
--
-- Implementing function in terms of 'nutrition'
-- requires 'Food' constraint

import Data.List (sortBy)
import Data.Ord (comparing)

sortByNutrition :: Food a => [a] -> [a]
--                 ^^^^^^^^^
sortByNutrition = sortBy (comparing nutrition)


-- {{{
class Food a where
  nutrition :: a -> Int
-- }}}
