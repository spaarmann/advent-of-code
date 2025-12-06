module Day2
    ( part1
    , part2
    , example
    ) where

import Util

import Data.Either
import GHC.Float
import Text.Parsec

example = "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124"

exp10, numDigits :: Int -> Int
exp10 = round . (10.0 **) . int2Double
numDigits = (+1) . floor . logBase 10.0 . int2Double
divides :: Int -> Int -> Bool
a `divides` b = (b `mod` a) == 0

repeatMult :: Int -> Int -> Int
repeatMult d l = sum $ map exp10 [0,l..d-1]

isRepeated :: Int -> Int -> Bool
isRepeated n l = repeatMult (numDigits n) l `divides` n

isInvalid :: Int -> Bool
isInvalid n
  | even (numDigits n) = isRepeated n (numDigits n `div` 2)
  | otherwise = False

isInvalid' :: Int -> Bool
isInvalid' n =
  let patternLengths = filter (`divides` numDigits n) [1..numDigits n `div` 2]
   in any (isRepeated n) patternLengths

expand (a, b) = [a..b]
ranges = parse (sepTuple "-" (read <$> many digit) `sepBy` char ',') ""

part1 = sum . concatMap (filter isInvalid . expand) . fromRight [] . ranges
part2 = sum . concatMap (filter isInvalid' . expand) . fromRight [] . ranges
