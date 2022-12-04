module Day04
    (
      doPart1,
      doPart2
    ) where

import Data.List.Split(splitOn)

fullyContains :: (Int, Int) -> (Int, Int) -> Bool
fullyContains (oneLower, oneUpper) (otherLower, otherUpper) =
  oneLower <= otherLower && otherUpper <= oneUpper

eitherFullyContains :: (Int, Int) -> (Int, Int) -> Bool
eitherFullyContains one other =
  fullyContains one other || fullyContains other one

doPart1 :: [Char] -> Int
doPart1 input =
  let allLines = lines input
      allPairs = map parseSections allLines
  in length $ filter (uncurry eitherFullyContains) allPairs

doPart2 :: [Char] -> Int
doPart2 input =
  0

parseSections :: String -> ((Int, Int), (Int, Int))
parseSections line =
  let [one, other] = splitOn "," line
  in (parseSection one, parseSection other)

parseSection :: String -> (Int, Int)
parseSection line =
  let [lower, upper] = splitOn "-" line
  in (read lower, read upper)
