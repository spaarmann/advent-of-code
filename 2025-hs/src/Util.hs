module Util
  ( sepTuple
  , exp10
  , numDigits
  , sepTuple'
  ) where

import Text.Parsec
import Text.Parsec.String
import GHC.Float

exp10, numDigits :: Int -> Int
exp10 = round . (10.0 **) . int2Double
numDigits = (+1) . floor . logBase 10.0 . int2Double

sepTuple :: String -> Parser a -> Parser (a, a)
sepTuple sep elemParser = do
  left <- elemParser
  _ <- string sep
  right <- elemParser
  return (left, right)

sepTuple' :: String -> Parser a -> Parser b -> Parser (a, b)
sepTuple' sep leftParser rightParser = do
  left <- leftParser
  _ <- string sep
  right <- rightParser
  return (left, right)
