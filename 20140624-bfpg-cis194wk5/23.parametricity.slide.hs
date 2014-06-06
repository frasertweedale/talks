-- Parametricity 5
--
-- What possible implementations could these functions have?

f :: a -> a
f = undefined

g :: a -> b
g = undefined

h :: a -> b -> a
h = undefined

i :: [a] -> [a]
i = undefined

j :: (b -> c) -> (a -> b) -> a -> c
j = undefined

k :: (a -> a) -> a -> a
k = undefined
