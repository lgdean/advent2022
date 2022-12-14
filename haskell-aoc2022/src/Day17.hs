module Day17
    (
      doPart1,
      hitsLeft,
      hitsRight,
      Shape (..),
      doPart2
    ) where

import Data.Set (Set, intersection)
import qualified Data.Set as Set

-- for today, coordinates start at 0,0 and go right (+x) and up (+y) from there.
-- the position of a shape is defined from bottom and left edges.
type Coord = (Int, Int)
chamberWidth :: Int
chamberWidth = 7
data Jet = JetLeft | JetRight deriving (Eq, Show)
data Shape = Dash | Plus | BackwardL | I | Square deriving (Eq, Show)

-- confusing name: these are actually about going through/into the wall!
hitsLeft :: Shape -> Coord -> Bool
hitsLeft _ (x,_) = x < 0

hitsRight :: Shape -> Coord -> Bool
hitsRight Dash (x,_) = chamberWidth <= x + 3
hitsRight Plus (x,_) = chamberWidth <= x + 2
hitsRight BackwardL (x,_) = (chamberWidth - 3) < x
hitsRight I (x,_) = (chamberWidth - 1) < x
hitsRight Square (x,_) = (chamberWidth - 2) < x

relativeCoords :: Shape -> Set Coord
relativeCoords Dash = Set.fromList [(x,0) | x <- [0..3]]
relativeCoords Plus = Set.fromList [(0,1), (1,0), (1,1), (1,2), (2,1)]
relativeCoords BackwardL = Set.fromList [(0,0), (1,0), (2,0), (2,1), (2,2)]
relativeCoords I = Set.fromList [(0,y) | y <- [0..3]]
relativeCoords Square = Set.fromList [(x,y) | x <- [0,1], y <- [0,1]]

overlap :: (Shape, Coord) -> (Shape, Coord) -> Bool
overlap (shapeA, posA) (shapeB, posB) =
  not $ null $ intersection (allCoords shapeA posA) (allCoords shapeB posB)

anyOverlap :: (Shape, Coord) -> [(Shape, Coord)] -> Bool
anyOverlap (shape, pos) prev =
  not $ null $ intersection (allCoords shape pos) (Set.unions $ map (uncurry allCoords) prev)

topOf :: Shape -> Coord -> Int
topOf shape pos =
  maximum $ Set.map snd $ allCoords shape pos -- or could hard-code by shape!

allCoords :: Shape -> Coord -> Set Coord
allCoords shape pos =
  Set.map (add pos) (relativeCoords shape)

add :: Coord -> Coord -> Coord
add (a,b) (x,y) = (a+x, b+y)

push :: Jet -> [(Shape, Coord)] -> Shape -> Coord -> Coord
push dir prev shape pos
  = result
  where
    hitsWall =
      case dir of
        JetLeft -> hitsLeft
        JetRight -> hitsRight
    nextPos = add pos (if dir == JetLeft then (-1,0) else (1,0)) -- clean up later
    overlaps = anyOverlap (shape, nextPos) prev -- try naive way first
    stopped = hitsWall shape nextPos || overlaps
    result = if stopped then pos else nextPos

fallOne :: [(Shape, Coord)] -> (Shape, Coord) -> Maybe Coord
fallOne prev (shape, pos) =
  let newPos@(_,y) = add pos (0,-1)
      stopped = y < 0 || anyOverlap (shape, newPos) prev -- try naive way first
  in if stopped then Nothing else Just newPos

fallDown :: Int -> [(Shape, Coord)] -> (Shape, Coord) -> [Jet] -> (Coord, [Jet])
fallDown lowerBound prev (shape, pos) jets =
  let pushResult = push (head jets) prev shape pos
      fallResult = fallOne prev (shape, pushResult)
  in case fallResult of
    Nothing -> (pushResult, tail jets)
    Just result -> fallDown lowerBound prev (shape, result) (tail jets)

allFallDown :: Int -> Int -> [(Shape, Coord)] -> [Jet] -> [Shape] -> Int
allFallDown currTop _ _ _ [] = currTop
allFallDown currTop currLower prevRocks jets (rock:restRocks) =
  let initPos = (2, 3+currTop)
      (rockRestingPos, unusedJets) = fallDown currTop prevRocks (rock, initPos) jets :: (Coord, [Jet])
      newTop = max currTop (topOf rock rockRestingPos + 1)
      newLower = currLower
  in allFallDown newTop newLower ((rock, rockRestingPos):prevRocks) unusedJets restRocks


doPart1 :: String -> Int
doPart1 input =
  let allLines = lines input
      jets = cycle $ parseLine $ head allLines
      rocks = take 2022 $ cycle [Dash, Plus, BackwardL, I, Square]
      initTop = 0
  in allFallDown initTop initTop [] jets rocks

doPart2 :: Int -> String -> Int
doPart2 howMany input =
  let allLines = lines input
      jets = cycle $ parseLine $ head allLines
      rocks = take howMany $ cycle [Dash, Plus, BackwardL, I, Square]
      initTop = 0
  in allFallDown initTop initTop [] jets rocks

parseLine :: String -> [Jet]
parseLine = map parseJet

-- or read I guess
parseJet :: Char -> Jet
parseJet '<' = JetLeft
parseJet '>' = JetRight
parseJet  x  = error (x : " is neither left nor right")
