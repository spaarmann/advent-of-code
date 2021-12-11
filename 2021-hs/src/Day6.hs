{-# LANGUAGE TupleSections #-}

module Day6
    ( part1
    , part2
    , example
    ) where

import Data.Array
import Data.List
import Data.List.Split
import Debug.Trace

example = "3,4,3,1,2"

emptyState = zip [0..8] (repeat 0)

parse :: String -> Array Int Int
-- There must be a better way of doing this?
parse = array (0,8) . map (\l -> (fst (head l), sum (map snd l))) . groupBy (\(a, _) (b, _) -> a == b) . sortBy (\(a, _) (b, _) -> compare a b) . (++) emptyState . map ((,1) . read) . splitOn ","

rotate :: Array Int Int -> Array Int Int
rotate a = array (0,8) ([(i, a!(i+1)) | i <- [0..7]] ++ [(8, a!0)])

updateState :: Array Int Int -> Array Int Int
updateState state = rotate state // [(6, state!0 + state!7)]

applyN :: Int -> (a -> a) -> a -> a
applyN n f = foldr (.) id (replicate n f)

part1 = sum . applyN 80 updateState . parse
part2 = sum . applyN 256 updateState . parse
