module Day18
    (
      doPart1,
      doPart2
    ) where

import Data.List ((\\), groupBy, partition, sort)
import Data.List.Split (splitOn)

type Cube = (Int, Int, Int)

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

part1SurfaceArea :: [Cube] -> Int
part1SurfaceArea cubes =
  let sharedSides = nAdjacentCubes cubes
      totalSides = 6 * length cubes
  in totalSides - 2 * sharedSides

doPart1 :: String -> Int
doPart1 input =
  let allLines = lines input
      cubes = map parseLine allLines
  in part1SurfaceArea cubes

doPart2 :: String -> Int
doPart2 input =
  let allLines = lines input
      cubes = map parseLine allLines
      maxX = maximum $ map (\(x,_,_) -> x) cubes
      maxY = maximum $ map (\(_,y,_) -> y) cubes
      maxZ = maximum $ map (\(_,_,z) -> z) cubes
      dropletAndAroundIt = [(x,y,z) | x <- [(-1)..maxX+1], y <- [(-1)..maxY+1], z <- [(-1)..maxZ+1]]
      somePointOutsideDroplet = head dropletAndAroundIt
      (_notPartOfDroplet, airInside) = collectClump [somePointOutsideDroplet] [] (tail dropletAndAroundIt \\ cubes)
  in part1SurfaceArea cubes - part1SurfaceArea airInside

_collectClumps :: [Cube] -> [[Cube]]
_collectClumps [] = []
_collectClumps (x:rest) =
  let (firstClump, remaining) = collectClump [x] [] rest
  in firstClump : _collectClumps remaining

-- there are of course more efficient ways to do this collection!
-- and I gather (haha) there are simpler ways to solve this problem, anyway.
collectClump :: [Cube] -> [Cube] -> [Cube] -> ([Cube], [Cube])
collectClump currGen prevGens [] = (currGen ++ prevGens, []) -- no more to collect? all done!
collectClump [] prevGens candidates = (prevGens, candidates) -- no more to check? all done with this clump!
collectClump currGen prevGens candidates =
  let (nextGen, remainingCandidates) = foldl grabAndCombine ([], candidates) currGen
  in collectClump nextGen (currGen ++ prevGens) remainingCandidates

grabAndCombine :: ([Cube], [Cube]) -> Cube -> ([Cube], [Cube])
grabAndCombine (found, candidates) curr =
  let (newlyFound, remainingCandidates) = partition (adjacent curr) candidates
  in (newlyFound ++ found, remainingCandidates)


parseLine :: String -> Cube
parseLine input =
  let parts = splitOn "," input
  in case parts of
    [x,y,z] -> (read x, read y, read z)
    _ -> error ("cannot parse line: " ++ input)
