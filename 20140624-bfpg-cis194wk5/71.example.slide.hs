-- Example: 'Food' type class
--

class Food a where
  nutrition :: a -> Nutrition

instance Food Apple where
  nutrition (Apple mass) = mass `div` 2       -- 1/2 of mass is nutrition

instance Food Orange where
  nutrition (Orange mass) = mass * 3 `div` 4  -- 3/4 of mass is nutrition

-- {{{
type Mass = Int
type Nutrition = Int

data Apple = Apple Mass deriving (Eq, Ord)
data Orange = Orange Mass deriving (Eq, Ord)
-- }}}
