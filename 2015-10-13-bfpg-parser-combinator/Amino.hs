{-# LANGUAGE FlexibleContexts #-}

module Amino where

import Control.Applicative
import Data.List.NonEmpty (NonEmpty)
import Control.Lens (Cons)

import Parser

data Amino = G | A | T | C
  deriving (Show, Eq)

-- |
-- >>> runParser parseHackerGene [A,A,C,C,G]
-- Just (A :| [A,C,C],[G])
--
parseHackerGene
  :: Cons s s Amino Amino
  => Parser s (NonEmpty Amino)
parseHackerGene = many1 (symbol A <|> symbol C)
