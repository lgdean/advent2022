module Day23
    (
      doPart1,
      doPart2
    ) where

import Data.Maybe (catMaybes)
import Data.Set (Set, size)
import qualified Data.Set as Set

import Debug.Trace (trace)

data Dir = North | South | East | West deriving (Eq, Show)
type Coord = (Int, Int)
type ElfSetup = Set Coord

-- this could be optimized in several ways: data structure stuff,
-- or knowing that only nearby elves can interfere with each other's moves (for now)
doRound :: ElfSetup -> [Dir] -> ElfSetup
doRound elves dirOrder =
  let proposedMoves = Set.map (\e -> (proposeMove elves dirOrder e, e)) elves
  in Set.fromList $ moveIfNoDupes $ Set.toAscList proposedMoves

-- ascending list
moveIfNoDupes :: [(Coord, Coord)] -> [Coord]
moveIfNoDupes [] = []
moveIfNoDupes [(dest,_)] = [dest]
moveIfNoDupes ((dest1,src1):(dest2,src2):rest)
  | dest1 /= dest2 = dest1 : moveIfNoDupes ((dest2,src2):rest)
  | otherwise      = let (dupes, moreToTry) = span ((== dest1) . fst) rest
                       in [src1, src2] ++ map snd dupes ++ moveIfNoDupes moreToTry

-- can optimize later if needed
proposeMove :: ElfSetup -> [Dir] -> Coord -> Coord
proposeMove setup dirs starting =
  let neighbors = Set.intersection setup $ Set.fromList $ neighborCoords starting
      canMove dir = not (any (`Set.member` setup) (positionsToward dir starting))
      allowedDirs = filter canMove dirs
  in
    if Set.null neighbors || null allowedDirs
    then starting
    else moveOne (head allowedDirs) starting

positionsToward :: Dir -> Coord -> [Coord]
positionsToward North (x,y) = map (add (x,y)) [(-1,-1), (0,-1), (1,-1)]
positionsToward South (x,y) = map (add (x,y)) [(-1,1), (0,1), (1,1)]
positionsToward East (x,y) = map (add (x,y)) [(1,-1), (1,0), (1,1)]
positionsToward West (x,y) = map (add (x,y)) [(-1,-1), (-1,0), (-1,1)]

moveOne :: Dir -> Coord -> Coord
moveOne North (x,y) = (x,y-1)
moveOne South (x,y) = (x,y+1)
moveOne East (x,y) = (x+1,y)
moveOne West (x,y) = (x-1,y)

nEmptyGroundTiles :: ElfSetup -> Int
nEmptyGroundTiles elves =
  let minX = minimum $ Set.map fst elves
      maxX = maximum $ Set.map fst elves
      minY = minimum $ Set.map snd elves
      maxY = maximum $ Set.map snd elves
      rectWidth = maxX - minX + 1
      rectHeight = maxY - minY + 1
      rectArea = trace (show $ zip ["width", "height"] [rectWidth, rectHeight]) $ rectWidth * rectHeight
  in rectArea - size elves

doPart1 :: String -> Int
doPart1 input =
  let allLines = lines input
      -- starting with (1,1) just because yesterday's puzzle did
      elfRows = zipWith parseLine [1..] allLines
      elfScan = Set.fromList $ concat elfRows :: ElfSetup
      initDirOrder = [North, South, West, East]
      dirOrders = iterate nextDirOrder initDirOrder
      manyRounds = scanl doRound elfScan dirOrders
      tenthRoundResult = manyRounds !! 10
  in nEmptyGroundTiles tenthRoundResult

doPart2 :: String -> Int
doPart2 input =
  let allLines = lines input
      -- starting with (1,1) just because yesterday's puzzle did
      elfRows = zipWith parseLine [1..] allLines
      elfScan = Set.fromList $ concat elfRows :: ElfSetup
      initDirOrder = [North, South, West, East]
      dirOrders = iterate nextDirOrder initDirOrder
      manyRounds = scanl doRound elfScan dirOrders
  in roundWithNoChange 1 manyRounds

parseLine :: Int -> String -> [Coord]
parseLine y input =
  let points = zipWith (\x c -> if c=='#' then Just (x,y) else Nothing) [1..] input
  in catMaybes points

-- ok but I do keep copying this one
add :: Coord -> Coord -> Coord
add (a,b) (x,y) = (a+x, b+y)

-- also this one, but there are 2 versions of it
neighborCoords :: (Int, Int) -> [(Int, Int)]
neighborCoords (x,y) =
  [add (x, y) (a, b) |
   a <- [- 1, 0, 1], b <- [- 1, 0, 1], (a, b) /= (0, 0)]

nextDirOrder :: [a] -> [a]
nextDirOrder [] = error "oh no I made a bug"
nextDirOrder (d:ds) = ds ++ [d]

roundWithNoChange :: Eq a => Int -> [a] -> Int
roundWithNoChange x [] = x -- or error
roundWithNoChange x [_] = x -- or error
roundWithNoChange x (a:b:rest)
  | a == b = x
  | otherwise = roundWithNoChange (x+1) (b:rest)
