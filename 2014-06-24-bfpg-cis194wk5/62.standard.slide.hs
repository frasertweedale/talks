-- Standard type classes: Show
--
-- Convert value into String

class Show a where
  showsPrec :: Int -> a -> ShowS
  show      :: a -> String
  showList  :: [a] -> ShowS

-- Normally just define 'show', or derive instance
