module Day18
    (
      doPart1,
      doPart2
    ) where

import Data.List (groupBy, sort)
import Data.List.Split (splitOn)

type Cube = (Int, Int, Int)

-- may not need it, but glad I thought it through
adjacent :: Cube -> Cube -> Bool
adjacent (a,b,c) (x,y,z) =
  let diffs = map abs [a-x, b-y, c-z]
  in sum diffs == 1

nAdjacentCubes :: [Cube] -> Int
nAdjacentCubes cubes =
  let asIs = nAdjacentThisWay cubes
      anotherWay = nAdjacentThisWay $ map (\(x,y,z)->(y,z,x)) cubes
      thirdWay = nAdjacentThisWay $ map (\(x,y,z)->(z,x,y)) cubes
  in asIs + anotherWay + thirdWay

nAdjacentThisWay :: [Cube] -> Int
nAdjacentThisWay cubes =
  let sortedCubes = sort cubes
      -- find just the ones with same X,Y and adjacent Z
      grouped = groupBy sameXY sortedCubes :: [[Cube]]
      justZs = map (map (\(_,_,z) -> z)) grouped :: [[Int]]
      howManyAdjacentZs = map nAdjacent justZs
  in sum howManyAdjacentZs

sameXY :: Cube -> Cube -> Bool
sameXY (a,b,_) (x,y,_) = (a,b) == (x,y)

nAdjacent :: [Int] -> Int
nAdjacent [] = 0
nAdjacent xs =
  let p a b = a+1 == b
  in length $ filter id $ zipWith p xs (tail xs)

doPart1 :: String -> Int
doPart1 input =
  let allLines = lines input
      cubes = map parseLine allLines
      sharedSides = nAdjacentCubes cubes
      totalSides = 6 * length cubes
  in totalSides - 2 * sharedSides

doPart2 :: String -> Int
doPart2 _ = 0


parseLine :: String -> Cube
parseLine input =
  let parts = splitOn "," input
  in case parts of
    [x,y,z] -> (read x, read y, read z)
    _ -> error ("cannot parse line: " ++ input)
