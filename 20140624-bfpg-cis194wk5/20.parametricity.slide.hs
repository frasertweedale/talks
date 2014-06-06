-- Parametricity 1
--

someFunction :: a -> a -> a
someFunction = undefined
--someFunction x y = x && y

-- NOTES
-- * Caller of a polymorphic function chooses the type
-- * Type variable in type signature is a *promise* that
--   function will work for any type
