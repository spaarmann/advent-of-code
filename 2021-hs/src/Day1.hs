module Day1
    ( part1
    , part2
    , example
    ) where

example = "199\n\
        \200\n\
        \208\n\
        \210\n\
        \200\n\
        \207\n\
        \240\n\
        \269\n\
        \260\n\
        \263"

windows2 xs = zip xs $ tail xs

part1 input = length $ filter (uncurry (<)) $ windows2 (map read $ lines input :: [Integer])

part2 input = length $ filter (uncurry (<)) $ helper (map read $ lines input :: [Integer])
        where helper xs = zip xs $ drop 3 xs
