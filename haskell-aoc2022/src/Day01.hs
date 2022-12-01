module Day01
    (
      doPart1,
      doPart2
    ) where

import Data.List (sort)
import Data.List.Split (splitOn)

import Lib

parseCalLists :: String -> [[Int]]
parseCalLists input =
  let allLines = lines input
      chunks = splitOn [""] allLines
  in map (map read) chunks

doPart1 :: [Char] -> Int
doPart1 input =
  let inputInts = parseCalLists input :: [[Int]]
      sums = map sum inputInts :: [Int]
  in foldl max 0 sums

doPart2 :: [Char] -> Int
doPart2 input =
  let inputInts = parseCalLists input :: [[Int]]
      sums = map sum inputInts :: [Int]
  in sum $ take 3 $ reverse $ sort sums
