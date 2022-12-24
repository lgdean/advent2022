module Day24
    (
      doPart1,
      doPart2
    ) where

import Data.List (elemIndices, findIndices, transpose)
import Data.Set (Set)
import qualified Data.Set as Set

data Dir = North | South | East | West deriving (Eq, Show)
type Coord = (Int, Int)
type Valley = [[Char]] -- for now

doRound :: Valley -> Set Coord -> Int -> Set Coord
doRound valley currPosSet minute =
  let candidateMoves = id : map moveOne [North, South, West, East]
      candidatePositions = Set.unions $ map (`Set.map` currPosSet) candidateMoves
  in Set.filter (isPositionOpen valley minute) candidatePositions

moveOne :: Dir -> Coord -> Coord
moveOne North (x,y) = (x,y-1)
moveOne South (x,y) = (x,y+1)
moveOne East (x,y) = (x+1,y)
moveOne West (x,y) = (x-1,y)

isPositionOpen :: Valley -> Int -> Coord -> Bool
isPositionOpen _ _ (0,-1) = True -- starting position is always OK
isPositionOpen valleyRows minute (x,y)
  | length valleyRows == y && length (head valleyRows) == x+1 = True -- goal position is always ok
  | otherwise =
      freeOfRowBlizzards valleyRows minute (x,y) && freeOfColumnBlizzards valleyRows minute (x,y)

blizzardsInit :: Char -> [Char] -> Set Int
blizzardsInit dirChar row =
  Set.fromList $ elemIndices dirChar row

freeOfRowBlizzardsGeneral :: (Char, Char) -> Valley -> Int -> Coord -> Bool
freeOfRowBlizzardsGeneral (incChar, decChar) valleyRows minute (x,y) =
  let width = length $ head valleyRows
      height = length valleyRows
      isWithinValley = 0 <= x && x < width && 0 <=y && y < height
      rightBlizzards = blizzardsInit incChar (valleyRows !! y)
      leftBlizzards = blizzardsInit decChar (valleyRows !! y)
      freeOfRight = ((x - minute) `mod` width) `Set.notMember` rightBlizzards
      freeOfLeft = ((x + minute) `mod` width) `Set.notMember` leftBlizzards
  in isWithinValley && freeOfRight && freeOfLeft

freeOfRowBlizzards :: Valley -> Int -> Coord -> Bool
freeOfRowBlizzards = freeOfRowBlizzardsGeneral ('>', '<')

freeOfColumnBlizzards :: Valley -> Int -> Coord -> Bool
freeOfColumnBlizzards valleyRows minute (x,y) =
  freeOfRowBlizzardsGeneral ('v','^') (transpose valleyRows) minute (y,x)

doPart1 :: String -> Int
doPart1 input =
  let allLines = lines input
      height = length allLines - 2
      valleyRowsPlusWalls = take height $ tail allLines
      width = length (head valleyRowsPlusWalls) - 2
      valleyRows = map (take width . tail) valleyRowsPlusWalls
      expeditionDest = (width-1, height-1) -- and then you can exit the following minute yay!
      justBeforeSuccess = howLongFromTo valleyRows (0,-1) expeditionDest 1
  in 1 + justBeforeSuccess

doPart2 :: String -> Int
doPart2 input =
  let allLines = lines input
      height = length allLines - 2
      valleyRowsPlusWalls = take height $ tail allLines
      width = length (head valleyRowsPlusWalls) - 2
      valleyRows = map (take width . tail) valleyRowsPlusWalls
      expeditionDest = (width-1, height-1) -- and then you can exit the following minute yay!
      firstRoundResult = 1 + howLongFromTo valleyRows (0,-1) expeditionDest 1
      secondRoundResult = firstRoundResult + howLongFromTo valleyRows (width-1, height) (0,0) firstRoundResult
      thirdRoundResult = secondRoundResult + howLongFromTo valleyRows (0,-1) expeditionDest secondRoundResult
  in thirdRoundResult

howLongFromTo :: Valley -> Coord -> Coord -> Int -> Int
howLongFromTo valleyRows start expeditionDest startMinute =
  let expeditionStart = Set.singleton start
      roundResults = scanl (doRound valleyRows) expeditionStart [startMinute..] :: [Set Coord]
      success = findIndices (expeditionDest `Set.member`) roundResults
  in head success
