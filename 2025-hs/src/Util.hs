module Util
  ( sepTuple
  ) where

import Text.Parsec
import Text.Parsec.String

sepTuple :: String -> Parser a -> Parser (a, a)
sepTuple sep elemParser = do
  left <- elemParser
  _ <- string sep
  right <- elemParser
  return (left, right)
