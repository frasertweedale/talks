main :: IO ()
main = print $ take 5 $ (reverse . reverse) [1,2,3,4,5]
