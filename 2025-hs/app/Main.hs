module Main (main) where

import Day1
import Day2
import Day3
-- MARK:IMPORTS

import System.Environment (getArgs)
import System.Exit (exitFailure)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["day1", "1"] -> readFile "input/day1" >>= print . Day1.part1
    ["day1", "1", "e"] -> print $ Day1.part1 Day1.example
    ["day1", "2"] -> readFile "input/day1" >>= print . Day1.part2
    ["day1", "2", "e"] -> print $ Day1.part2 Day1.example
    ["day2", "1"] -> readFile "input/day2" >>= print . Day2.part1
    ["day2", "1", "e"] -> print $ Day2.part1 Day2.example
    ["day2", "2"] -> readFile "input/day2" >>= print . Day2.part2
    ["day2", "2", "e"] -> print $ Day2.part2 Day2.example
    ["day3", "1"] -> readFile "input/day3" >>= print . Day3.part1
    ["day3", "1", "e"] -> print $ Day3.part1 Day3.example
    ["day3", "2"] -> readFile "input/day3" >>= print . Day3.part2
    ["day3", "2", "e"] -> print $ Day3.part2 Day3.example
    -- MARK:DAYS
    _ -> exitFailure
