{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Parser where

import Control.Applicative
import Data.Bifunctor (first)
import Data.Char (isDigit)
import Data.List.NonEmpty (NonEmpty(..), toList)
import Control.Monad ((>=>))

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }
