-- Example: Apples and Oranges
--

type Mass = Int
type Nutrition = Int

data Apple = Apple Mass deriving (Eq, Ord)
data Orange = Orange Mass deriving (Eq, Ord)
