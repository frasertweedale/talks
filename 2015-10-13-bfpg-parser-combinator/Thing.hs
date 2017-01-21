module Thing where

import Control.Applicative

import Parser

data Thing = Thing Int Bool
  deriving (Show, Eq)

-- |
-- >>> runParser parseThing "555t"
-- Just (Thing 555 True,"")
--
-- >>> runParser parseThing "t555"
-- Nothing
--
-- >>> runParser parseThing "7f..."
-- Just (Thing 7 False,"...")
--
-- >>> runParser parseThing "7 f"
-- Nothing
--
parseThing :: Parser Thing
parseThing = Thing <$> parseInt <*> parseBool
