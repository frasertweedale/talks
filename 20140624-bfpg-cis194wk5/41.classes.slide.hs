-- Type classes 2: anatomy of a type class
--

class Eq a where             -- type class with single parameter
  (==) :: a -> a -> Bool     -- two functions
  (/=) :: a -> a -> Bool

-- NOTES:
--
-- * functions may have a default implementation
-- * default implementations can use the other functions
-- * default implementations may be overridden
