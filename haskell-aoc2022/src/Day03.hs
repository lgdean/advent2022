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
  let [f,s] = chunksOf ((length lst) `div` 2) lst
      findIt (x:xs) (y:ys) = if x==y then x else (if x<y then findIt xs (y:ys) else findIt (x:xs) ys)
  in findIt (sort f) (sort s)

doPart1 :: [Char] -> Int
doPart1 input =
  let allLines = lines input
      commonItems = map commonItem allLines
  in sum $ map priority commonItems

doPart2 :: [Char] -> Int
doPart2 input =
  0

priority :: Char -> Int
priority ch
  | isAsciiLower ch = ord ch - ord 'a' + 1
  | isAsciiUpper ch = ord ch - ord 'A' + 27
  | otherwise = error "unknown char"
