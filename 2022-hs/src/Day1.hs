module Day1
    ( part1
    , part2
    , example
    ) where

import Data.List
import Data.List.Split (splitOn)

example = "1000\n\
          \2000\n\
          \3000\n\
          \\n\
          \4000\n\
          \\n\
          \5000\n\
          \6000\n\
          \\n\
          \7000\n\
          \8000\n\
          \9000\n\
          \\n\
          \10000"

elfSums = map (sum . map read . lines) . splitOn "\n\n"
part1 = maximum . elfSums
part2 = sum . take 3 . reverse . sort . elfSums
