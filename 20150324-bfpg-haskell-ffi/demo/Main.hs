import Binding

main :: IO ()
main = do
  thing <- makeThing 42 "o=BFPG"
  case thing of
    Nothing -> putStrLn "failed to make a thing"
    Just thing' -> do
      name <- getName thing'
      let serial = getSerial thing'
      putStrLn $ "made a thing: " ++ show serial ++ " " ++ name
