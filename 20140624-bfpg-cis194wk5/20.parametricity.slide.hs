-- Parametricity 1
--

-- | some function
--
f :: a -> a -> a
f = undefined
--f x y = x && y

-- NOTES
-- * Caller of a polymorphic function chooses the type
-- * Type variable in type signature is a *promise* that
--   function will work for any type
