{-# LANGUAGE TupleSections #-}

module Day4
    ( part1
    , part2
    , example
    ) where

import Data.Bifunctor
import Data.List
import Data.List.Split
import Debug.Trace

example = "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1\n\
                        \\n\
          \22 13 17 11  0\n\
          \ 8  2 23  4 24\n\
          \21  9 14 16  7\n\
          \ 6 10  3 18  5\n\
          \ 1 12 20 15 19\n\
                        \\n\
          \ 3 15  0  2 22\n\
          \ 9 18 13 17  5\n\
          \19  8  7 25 23\n\
          \20 11 10 24  4\n\
          \14 21 16 12  6\n\
                        \\n\
          \14 21 17 24  4\n\
          \10 16 15  9 19\n\
          \18  8 23 26 20\n\
          \22 11 13  6  5\n\
          \ 2  0 12  3  7"

newtype BingoBoard = Board [(Bool, Int)]
  deriving Show

markNumber :: BingoBoard -> Int -> BingoBoard
markNumber (Board board) n = Board $ map (\(checked, num) -> (checked || num == n, num)) board

hasWon :: BingoBoard -> Bool
hasWon (Board board) = let rows = chunksOf 5 (map fst board) in
                           any and rows || any and (transpose rows)

score :: BingoBoard -> Int
score (Board board) = sum $ map snd $ filter (not . fst) board

boardFromInts :: [Int] -> BingoBoard
boardFromInts nums = Board $ map (False,) nums

parseBoard :: String -> BingoBoard
parseBoard str = boardFromInts $ map read (concatMap words $ lines str)

parseInput :: String -> ([Int], [BingoBoard])
parseInput str = case splitOn "\n\n" str of
                   (numbers : boards) -> (map read (splitOn "," numbers), map parseBoard boards)
                   _ -> error "parse error"

playBingo :: [Int] -> [BingoBoard] -> [(Int, [BingoBoard])]
playBingo nums boards = scanl (\(_, boards') num -> (num, map (`markNumber` num) boards')) (-1, boards) nums

-- This is neat, but I'm not quite happy with having to find the winning board out of a list again,
-- after having already figured out it is winning.
part1 input = let (nums, boards) = parseInput input in
                  case second (find hasWon) $ head $ dropWhile (not . any hasWon . snd) (playBingo nums boards) of
                    (num, Just winner) -> num * score winner
                    (num, Nothing) -> error "no board won!"

-- This avoids the problem from above, but at the expense of the `unpack` function, which I also
-- don't like very much.
part1Unpack input = let (nums, boards) = parseInput input in
                  case find (hasWon . snd) $ unpack (playBingo nums boards) of
                    Just (num, winner) -> num * score winner
                    Nothing -> error "no board won!"

unpack :: [(Int, [BingoBoard])] -> [(Int, BingoBoard)]
unpack = concatMap (\(n, boards) -> map (n,) boards) 

-- This was my original solution and is actually pretty okay, but I *really* like the concept of the
-- `playBingo` function and taking advantage of laziness, so...
part1Recursive input = let (nums, boards) = parseInput input in
            helper nums boards
  where
    helper (n : nums) boards = let boards' = map (`markNumber` n) boards in
                                   case find hasWon boards' of
                                     Just winner -> n * score winner
                                     Nothing -> helper nums boards'
    helper _ _ = error "no board won!"

part2 input = let (nums, boards) = parseInput input in
                  helper nums boards
  where
    helper :: [Int] -> [BingoBoard] -> Int
    helper (n : nums) [board] = let board' = markNumber board n in
                                    if hasWon board' then n * score board' else helper nums [board']
    helper (n : nums) boards = let boards' = filter (not . hasWon) (map (`markNumber` n) boards) in
                                   helper nums boards'
    helper _ _ = undefined
