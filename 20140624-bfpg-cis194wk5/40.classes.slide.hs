-- Type classes 1
--
-- NOTES:
--
-- * type classes correspond to *sets of types* with shared
--   operations
-- * functions can have *type class constraints*
-- * such functions only work with types that have *instances* of
--   the specified type classes
-- * often a single type class constraint, but sometimes more

(+) :: (Num a) => a -> a -> a
(+) = undefined
