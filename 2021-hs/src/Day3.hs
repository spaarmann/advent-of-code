module Day3
    ( part1
    , part2
    , example
    ) where

import Debug.Trace (trace)
import Data.Bits

example = "00100\n\
          \11110\n\
          \10110\n\
          \10111\n\
          \10101\n\
          \01111\n\
          \00111\n\
          \11100\n\
          \10000\n\
          \11001\n\
          \00010\n\
          \01010"

readBin :: String -> Integer
readBin ('0':s) = readBin s
readBin ('1':s) = 2 ^ length s + readBin s
readBin "" = 0
readBin _ = undefined

countBitsSet :: [String] -> Int -> Int
countBitsSet nums bit = length $ filter (\n -> n !! bit == '1') nums

calculateGamma nums = let numBits = length $ head nums
                          chooseBit numSet
                            | numSet > length nums `div` 2 = '1'
                            | otherwise = '0'
                      in
                      map (chooseBit . countBitsSet nums)  [0 .. (numBits - 1)]

part1 input = let nums = lines input
                  numBits = length $ head nums
                  gamma = readBin (calculateGamma nums)
                  epsilon = complement gamma .&. ((2 ^ numBits) - 1)
              in
  gamma * epsilon

part2 input = let nums = lines input
                  numBits = length $ head nums
                  oxyChooseBit candidates numSet
                    | numSet >= (length candidates - numSet) = '1'
                    | otherwise = '0'
                  co2ChooseBit candidates numSet
                    | numSet < (length candidates - numSet) = '1'
                    | otherwise = '0'
              in
                 readBin (helper oxyChooseBit 0 nums) * readBin (helper co2ChooseBit 0 nums)
  where
    helper _ _ [result] = result
    helper chooseBit bit candidates = let filterBit = chooseBit candidates (countBitsSet candidates bit)
                                      in helper chooseBit (bit + 1) (filter (\n -> n !! bit == filterBit) candidates)
