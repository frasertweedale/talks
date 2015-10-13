{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Parser where

import Control.Applicative
import Data.Char (isDigit)
import Data.List.NonEmpty (NonEmpty(..), toList)

-- import Data.Bifunctor (first)
--
first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

newtype Parser a = Parser
  { runParser :: String -> Maybe (a, String) }

-- |
-- >>> runParser (satisfy isDigit) ""
-- Nothing
--
-- >>> runParser (satisfy isDigit) "hi"
-- Nothing
--
-- >>> runParser (satisfy isDigit) "99 bottles of beer"
-- Just ('9',"9 bottles of beer")
--
satisfy :: (Char -> Bool) -> Parser Char
satisfy = undefined

-- |
-- >>> runParser (symbol 'a') "abc"
-- Just ('a',"bc")
--
-- >>> runParser (symbol 'a') "123"
-- Nothing
--
symbol :: Char -> Parser Char
symbol = undefined
