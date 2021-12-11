module Day7
    ( part1
    , part2
    , example
    ) where

import Data.List.Split
import Data.List

example = "16,1,2,0,4,2,7,1,2,14"

parse :: String -> [Int]
parse = map read . splitOn ","

part1Costs :: Int -> [Int] -> Int
part1Costs target positions = sum $ map (\p -> abs (p - target)) positions

median :: Ord a => [a] -> a
median xs = sort xs !! (length xs `div` 2)

part1 input = let positions = parse input
                  target = median positions
              in
                part1Costs target positions

part2Costs :: Int -> [Int] -> Int
part2Costs target positions = sum $ map (\p -> gaussSum (abs (p - target))) positions
  where gaussSum n = (n * (n + 1)) `div` 2

mean :: [Int] -> Float
mean xs = fromIntegral (sum xs) / fromIntegral (length xs)

part2 input = let positions = parse input
                  m = mean positions
                  ceilTarget = ceiling m
                  floorTarget = floor m
              in
                minimum [part2Costs ceilTarget positions, part2Costs floorTarget positions]
