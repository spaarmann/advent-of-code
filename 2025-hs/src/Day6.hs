module Day6
    ( part1
    , part2
    , example
    ) where
import Text.Parsec.String (Parser)
import Text.Parsec
import Data.Either
import Debug.Trace (traceShow)

example = "123 328  51 64 \n\
          \ 45 64  387 23 \n\
          \  6 98  215 314\n\
          \*   +   *   +  \n"

type Op = Int -> Int -> Int

parseOps = fromRight [] . parse (op `sepBy` skipMany1 space) ""
  where 
    op = do _ <- char '*'; return (*)
     <|> do _ <- char '+'; return (+)
parseInts = fromRight [] . parse ((read <$> many1 digit) `sepBy` skipMany1 space) ""

parseInput :: String -> ([Op], [[Int]])
parseInput input =
  let (ol:nls) = reverse $ lines input
   in (parseOps ol, map parseInts nls)

part1 input = traceShow (snd $ parseInput input) 0

part2 input = 0
