-- Example: type class constraint on instances
--

-- | List of 'Food' has overall nutrition, therefore a list of
--   'Food' can also be 'Food'.
--
instance Food a => Food [a] where
--       ^^^^^^^^^
  nutrition = sum . map nutrition

-- | Pair type '(a,b)' is 'Food' as long as both 'a' and 'b' are.
--
instance (Food a, Food b) => Food (a, b) where
  nutrition (a, b) = nutrition a + nutrition b


-- {{{
class Food a where
  nutrition :: a -> Int
-- }}}
