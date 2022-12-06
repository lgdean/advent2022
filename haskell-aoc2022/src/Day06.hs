module Day06
    (
      doPart1,
      doPart2
    ) where

import Data.Set (size)
import qualified Data.Set as Set

doPart1 :: [Char] -> Int
doPart1 = findMarker 4

findMarker :: Ord a => Int -> [a] -> Int
findMarker howMany input =
  findInner howMany input
  where findInner pos rest =
          let first4 = take howMany rest
          in if size (Set.fromList first4) == howMany
             then pos
             else findInner (pos+1) (tail rest)

doPart2 :: [Char] -> Int
doPart2 = findMarker 14
