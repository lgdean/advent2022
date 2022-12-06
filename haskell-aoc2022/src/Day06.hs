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
  let findInner pos rest =
        let first4 = take howMany rest
            set4 = Set.fromList first4
        in if Set.size set4 == howMany
           then pos
           else findInner (pos+1) (tail rest)
  in findInner howMany input

doPart2 :: [Char] -> Int
doPart2 = findMarker 14
