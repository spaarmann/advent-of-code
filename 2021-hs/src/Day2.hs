module Day2
    ( part1
    , part2
    , example
    ) where

example = "forward 5\n\
          \down 5\n\
          \forward 8\n\
          \up 3\n\
          \down 8\n\
          \forward 2"

parse input = map ((\[cmd, n] -> (cmd, read n :: Integer)) . words) $ lines input

part1 input = let (horiz, depth) = foldl apply_cmd (0, 0) (parse input) in horiz * depth
  where
    apply_cmd (horiz, depth) (cmd, n) = case cmd of
      "forward" -> (horiz + n, depth)
      "down" -> (horiz, depth + n)
      "up" -> (horiz, depth - n)
      _ -> undefined

part2 input = let (horiz, depth, aim) = foldl apply_cmd (0, 0, 0) (parse input) in horiz * depth
  where
    apply_cmd (horiz, depth, aim) (cmd, n) = case cmd of
      "forward" -> (horiz + n, depth + aim * n, aim)
      "down" -> (horiz, depth, aim + n)
      "up" -> (horiz, depth, aim - n)
      _ -> undefined
