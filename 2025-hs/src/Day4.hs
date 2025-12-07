module Day4
    ( part1
    , part2
    , example
    ) where

import qualified Data.Vector as V
import Data.Vector (Vector, (!), (!?), (//))

example = "..@@.@@@@.\n\
          \@@@.@.@.@@\n\
          \@@@@@.@.@@\n\
          \@.@@@@..@.\n\
          \@@.@@@@.@@\n\
          \.@@@@@@@.@\n\
          \.@.@.@.@@@\n\
          \@.@@@.@@@@\n\
          \.@@@@@@@@.\n\
          \@.@.@@@.@.\n"

type Grid = Vector (Vector Bool)
g `at` (x, y) = Just True == ((g !? y) >>= (!? x))

parse = V.fromList . fmap (V.fromList . fmap parseChar) . lines
  where
    parseChar '@' = True
    parseChar _ = False

adjacents = [(x, y) | x <- [-1, 0, 1], y <- [-1, 0, 1], not (x == 0 && y == 0)]
add (a, b) (c, d) = (a + c, b + d)

removableRolls g =
  let countAdjacent x y = length $ filter ((g `at`) . add (x, y)) adjacents
      removableInRow y = V.map (\(x, _) -> (x, y)) . V.filter (\(x, _) -> countAdjacent x y < 4) . V.filter snd . V.indexed
   in V.concatMap (uncurry removableInRow) $ V.indexed g

part1 = length . removableRolls . parse

removeRolls :: Grid -> Vector (Int, Int) -> Grid
removeRolls = V.foldl' removeRoll
  where
    removeRoll g' (x, y) = g' // [(y, g' ! y // [(x, False)])]

removeAllRolls :: Grid -> Int
removeAllRolls g =
  let rolls = removableRolls g
   in if null rolls then 0
                    else length rolls + removeAllRolls (removeRolls g rolls)

part2 = removeAllRolls . parse
