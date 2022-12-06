module Day06
    (
      doPart1,
      doPart2
    ) where

import Data.Set (Set)
import qualified Data.Set as Set

doPart1 :: [Char] -> Int
doPart1 = part1Inner 4

part1Inner :: Ord a => Int -> [a] -> Int
part1Inner pos input =
  let (first4, rest) = splitAt 4 input
      set4 = Set.fromList first4
  in if Set.size set4 == 4 then pos else part1Inner (pos+1) (tail input)

doPart2 :: [Char] -> Int
doPart2 = part2Inner 14

part2Inner :: Ord a => Int -> [a] -> Int
part2Inner pos input =
  let (first4, rest) = splitAt 14 input
      set4 = Set.fromList first4
  in if Set.size set4 == 14 then pos else part2Inner (pos+1) (tail input)
