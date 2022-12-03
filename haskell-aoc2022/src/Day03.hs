module Day03
    (
      doPart1,
      doPart2
    ) where

import Data.Char(isAsciiLower, isAsciiUpper, ord)
import Data.List(sort)
import Data.List.Split(chunksOf)

commonItem :: Ord a => [a] -> a
commonItem lst =
  let (f,s) = splitAt (length lst `div` 2) lst
  in head $ allCommon (sort f) (sort s)

doPart1 :: [Char] -> Int
doPart1 input =
  let allLines = lines input
      commonItems = map commonItem allLines
  in sum $ map priority commonItems

-- assumes sorted lists
allCommon :: Ord a => [a] -> [a] -> [a]
allCommon _ [] = []
allCommon [] _ = []
allCommon (x:xs) (y:ys)
  | x==y = x : allCommon xs ys
  | x < y = allCommon xs (y:ys)
  | otherwise = allCommon (x:xs) ys

doPart2 :: [Char] -> Int
doPart2 input =
  let elfGroups = chunksOf 3 $ lines input
      commonGroupItem elves = foldl1 allCommon $ map sort elves
      commonItems = map commonGroupItem elfGroups
  in sum $ map (priority . head) commonItems

priority :: Char -> Int
priority ch
  | isAsciiLower ch = ord ch - ord 'a' + 1
  | isAsciiUpper ch = ord ch - ord 'A' + 27
  | otherwise = error "unknown char"
