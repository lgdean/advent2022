module Day24
    (
      doPart1,
      doPart2
    ) where

import Data.List (elemIndices, findIndices, transpose)
import Data.Set (Set)
import qualified Data.Set as Set

import Debug.Trace (trace)

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
isPositionOpen valleyRows minute (x,y) =
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
      -- there is surely a simpler way to do this math
      freeOfRight = x `Set.notMember` Set.map (\a -> (a + minute) `mod` width) rightBlizzards
      freeOfLeft = x `Set.notMember` Set.map (\a -> (a - minute) `mod` width) leftBlizzards
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
      expeditionStart = Set.singleton (0,-1)
      expeditionDest = (width-1, height-1) -- and then you can exit the following minute yay!
      roundResults = scanl (doRound valleyRows) expeditionStart [1..] :: [Set Coord]
      justBeforeSuccess = findIndices (expeditionDest `Set.member`) roundResults
--  in trace (show $ zip ["width", "height"] [width, height]) $ 1 + head justBeforeSuccess
  in trace (show valleyRows) $ 1 + head justBeforeSuccess

doPart2 :: String -> Int
doPart2 input =
  let _allLines = lines input
  in 0
