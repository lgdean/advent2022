module Day06
    (
      doPart1,
      doPart2
    ) where

import Data.Set ()
import qualified Data.Set as Set

doPart1 :: [Char] -> Int
doPart1 = findMarker 4

findMarker :: Ord a => Int -> [a] -> Int
findMarker howMany input =
  let part1Inner pos rest =
        let first4 = take howMany rest
            set4 = Set.fromList first4
        in if Set.size set4 == howMany then pos else part1Inner (pos+1) (tail rest)
  in part1Inner howMany input

doPart2 :: [Char] -> Int
doPart2 = findMarker 14
