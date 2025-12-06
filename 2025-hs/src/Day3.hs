module Day3
    ( part1
    , part2
    , example
    ) where

import Data.Char (digitToInt)
import Util
import Data.List (tails)
import Data.Foldable (maximumBy)

example = "987654321111111\n\
          \811111111111119\n\
          \234234234234278\n\
          \818181911112111"

banks :: String -> [[Int]]
banks = map (map digitToInt) . lines

maxCombination :: (Ord a) => Int -> [a] -> [a]
maxCombination 1 xs = [maximum xs]
maxCombination n xs | length xs < n = []
maxCombination n xs = 
  -- `reverse` because maximumBy returns the right-most max instance
  let (y:ys) = maximumBy (\a b -> head a `compare` head b) . reverse $ filter ((>= n) . length) $ tails xs
   in y : maxCombination (n-1) ys

combine = sum . zipWith (*) (map exp10 [0..]) . reverse
part1 = sum . map (combine . maxCombination 2) . banks
part2 = sum . map (combine . maxCombination 12) . banks
