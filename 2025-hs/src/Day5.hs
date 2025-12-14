module Day5
    ( part1
    , part2
    , example
    ) where

import Text.Parsec
import Data.Either
import Util
import Data.Function
import Data.List (sortBy)

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
   in length $ filter (isFresh fresh) ingredients

mergeSingle :: (Int, Int) -> (Int, Int) -> Maybe (Int, Int)
mergeSingle (a, b) (c, d)
  | a <= c && c <= b = Just (a, max b d)
  | otherwise = Nothing

merge :: [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
merge [] f = [f]
merge (x:xs) f = case mergeSingle x f of
                   Just m -> m:xs
                   Nothing -> x : merge xs f

part2 = sum . map size . foldl merge [] . sortBy (compare `on` fst) . fst . parseInput
  where size (a, b) = b - a + 1
