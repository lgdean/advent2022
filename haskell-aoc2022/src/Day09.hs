module Day09
    (
      doPart1,
      doPart2
    ) where

import Data.Set ()
import qualified Data.Set as Set

data Dir = Up | Down | Lef | Righ
type RopePos = ((Int, Int), (Int, Int))

moveHead :: Dir -> (Int, Int) -> (Int, Int)
moveHead Up (x,y) = (x,y+1)
moveHead Down (x,y) = (x,y-1)
moveHead Lef (x,y) = (x-1,y)
moveHead Righ (x,y) = (x+1,y)

touching :: (Int, Int) -> (Int, Int) -> Bool
touching (hx, hy) (tx, ty) =
  let diffx = abs (hx - tx)
      diffy = abs (hy - ty)
  in diffx <= 1 && diffy <= 1

keepUpWith :: (Int, Int) -> (Int, Int) -> (Int, Int)
keepUpWith (hx, hy) (tx, ty)
  | touching (hx, hy) (tx, ty) = (tx, ty)
  | hx > tx && hy > ty         = (tx+1, ty+1)
  | hx > tx && hy < ty         = (tx+1, ty-1)
  | hx < tx && hy > ty         = (tx-1, ty+1)
  | hx < tx && hy < ty         = (tx-1, ty-1)
  | hx > tx = (tx+1, ty)
  | hx < tx = (tx-1, ty)
  | hy > ty = (tx, ty+1)
  | hy < ty = (tx, ty-1)
  | otherwise = error "bug in logic, oh no"

doMove :: Dir -> RopePos -> RopePos
doMove dir (initHead, initTail) =
  let newHead = moveHead dir initHead
      newTail = keepUpWith newHead initTail
  in (newHead, newTail)

doPart1 :: String -> Int
doPart1 input =
  let allLines = lines input
      allMoves = concatMap parseLine allLines
      allRopePositions = scanl (flip doMove) ((0,0),(0,0)) allMoves
      allTails = map snd allRopePositions
  in Set.size $ Set.fromList allTails

doPart2 :: String -> Int
doPart2 _ = 0

parseLine :: String -> [Dir]
parseLine line =
  case words line of
    ["U", x] -> replicate (read x) Up
    ["D", x] -> replicate (read x) Down
    ["L", x] -> replicate (read x) Lef
    ["R", x] -> replicate (read x) Righ
    _        -> error ("failed to parse line: " ++ line)
