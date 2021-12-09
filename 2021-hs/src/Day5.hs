module Day5
    ( part1
    , part2
    , example
    ) where

import Data.List
import Data.List.Split

example = "0,9 -> 5,9\n\
          \8,0 -> 0,8\n\
          \9,4 -> 3,4\n\
          \2,2 -> 2,1\n\
          \7,0 -> 7,4\n\
          \6,4 -> 2,0\n\
          \0,9 -> 2,9\n\
          \3,4 -> 1,4\n\
          \0,0 -> 8,8\n\
          \5,5 -> 8,2\n"

boardSize = 1000

data Point = Point Int Int
  deriving(Eq, Ord, Show)
data Line = Line Point Point
  deriving Show

parsePoint :: String -> Point
parsePoint str = case splitOn "," str of
                   [x, y] -> Point (read x) (read y)
                   _ -> error "parsePoint failed"

parseLine :: String -> Line
parseLine str = case splitOn " -> " str of
                  [start, end] -> Line (parsePoint start) (parsePoint end)
                  _ -> error "parseLine failed"

parseInput :: String -> [Line]
parseInput = map parseLine . lines

range :: Int -> Int -> [Int]
range a b
  | a < b = [a .. b]
  | a > b = reverse [b .. a]
  | otherwise = []

getPoints :: Line -> [Point]
getPoints (Line (Point x1 y1) (Point x2 y2))
  | x1 == x2 = map (Point x1) $ range y1 y2
  | y1 == y2 = map (`Point` y1) $ range x1 x2
  | otherwise = zipWith Point (range x1 x2) (range y1 y2)

isDiagonal :: Line -> Bool
isDiagonal (Line (Point x1 y1) (Point x2 y2)) = x1 /= x2 && y1 /= y2

part1 = length . filter (>= 2) . map length . group . sort . concatMap getPoints . filter (not . isDiagonal) . parseInput

part2 = length . filter (>= 2) . map length . group . sort . concatMap getPoints . parseInput
