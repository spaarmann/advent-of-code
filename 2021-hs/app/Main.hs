module Main where

import Day1

main :: IO ()
main = do
    input <- readFile "../2021/input/day1"
    print $ Day1.part2 input
