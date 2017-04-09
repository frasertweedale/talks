{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import Criterion.Main
import Criterion.Types

import Demo

main :: IO ()
main = defaultMainWith (defaultConfig { timeLimit = 1 })
  [
  ]

s = T.replicate 1000 "F"
s' = T.replicate 1000000 "F"
