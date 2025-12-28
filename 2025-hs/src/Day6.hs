module Day6
    ( part1
    , part2
    , example
    ) where

import Text.Parsec
import Data.Either
import Data.List (transpose)

example = "123 328  51 64 \n\
          \ 45 64  387 23 \n\
          \  6 98  215 314\n\
          \*   +   *   +  \n"

type Op = Int -> Int -> Int

parseOps = fromRight [] . parse (op `endBy` skipMany1 space) ""
  where
    op = do _ <- char '*'; return ((*), 1)
     <|> do _ <- char '+'; return ((+), 0)
parseInts = fromRight [] . parse parser ""
  where
    parser = do spaces; (read <$> many1 digit) `endBy` spaces;

parseInput :: String -> ([(Op, Int)], [[Int]])
parseInput input =
  let (ol:nls) = reverse $ lines input
   in (parseOps ol, transpose $ map parseInts nls)

part1 = sum . uncurry (zipWith (uncurry foldl)) . parseInput

part2 input = 0
