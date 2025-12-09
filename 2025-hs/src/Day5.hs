module Day5
    ( part1
    , part2
    , example
    ) where

import Text.Parsec
import Data.Either
import qualified Data.Set as S
import Util
import Debug.Trace

example = "3-5\n\
          \10-14\n\
          \16-20\n\
          \12-18\n\
          \\n\
          \1\n\
          \5\n\
          \8\n\
          \11\n\
          \17\n\
          \32\n"


parseInput :: String -> ([(Int, Int)], [Int])
parseInput = fromRight ([], []) . parse parser ""
  where
    ingredient = read <$> many digit
    range = sepTuple "-" ingredient
    ranges = range `endBy` string "\n"
    ingredients = ingredient `endBy` string "\n"
    parser = sepTuple' "\n" ranges ingredients

isFresh :: [(Int, Int)] -> Int -> Bool
isFresh fresh i = any (contains i) fresh
  where contains x (a, b) = a <= x && b >= x

part1 input = 
  let (fresh, ingredients) = parseInput input
   in traceShow (minimum $ map fst fresh, maximum $ map snd fresh) $ length $ filter (isFresh fresh) ingredients


expand (a, b) = S.fromAscList [a..b]
part2 = length . S.unions . map expand . fst . parseInput
