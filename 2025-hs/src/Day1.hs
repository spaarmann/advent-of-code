module Day1
    ( part1
    , part2
    , example
    ) where

example = "L68\n\
  \L30\n\
  \R48\n\
  \L5\n\
  \R60\n\
  \L55\n\
  \L1\n\
  \L99\n\
  \R14\n\
  \L82"

offset ('L':num) = -(read num)
offset ('R':num) = read num

wrap f a b = (f a b) `mod` 100
zeroHits = length . filter (== 0) . scanl (wrap (+)) 50

part1 = zeroHits . map offset . lines
part2 = zeroHits . concatMap (\o -> replicate (abs o) (signum o)) . map offset . lines
