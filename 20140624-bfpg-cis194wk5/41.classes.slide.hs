-- Type classes 2: anatomy
--

class Eq a where             -- type class with single parameter
  (==) :: a -> a -> Bool     -- two functions
  (/=) :: a -> a -> Bool

-- NOTES:
--
-- * functions may have a default implementation
--
-- * default implementations can use the other functions
--
-- * default implementations may be overridden
--
-- * minimal complete definition warnings in GHC >= 7.8.1
