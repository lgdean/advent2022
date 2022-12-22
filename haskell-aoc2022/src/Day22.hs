module Day22
    (
      doPart1,
      doPart2
    ) where

import Data.Char (isDigit)
import Data.Maybe (fromJust, isJust)

import Lib (mapSnd, strip)

import Debug.Trace (trace)

type Coord = (Int, Int)
data Tile = Open | Wall deriving (Eq, Show)
data Move = Forward Int | TurnLeft | TurnRight deriving (Eq, Show)
data FacingDir = Up | Righ | Down | Lef deriving (Eq, Show)

type Board = [[(Coord, Tile)]]

rowFacingRight :: Int -> Board -> [(Coord, Tile)]
rowFacingRight y board = board !! (y-1) -- or look up by coords? but this is faster

columnFacingDown :: Int -> Board -> [(Coord, Tile)]
columnFacingDown x board =
  -- why am I not just using a Map at this point? oh well.
  let candidates = map (dropWhile ((/= x) . fst . fst)) board
      useful = filter (not . null) candidates
      answer = map head useful
  in answer

rowOrCol :: (Coord, Tile) -> FacingDir -> Board ->  [(Coord, Tile)]
rowOrCol ((x,_),_) Down = columnFacingDown x
rowOrCol ((x,_),_) Up = reverse . columnFacingDown x
rowOrCol ((_,y),_) Righ = rowFacingRight y
rowOrCol ((_,y),_) Lef = reverse . rowFacingRight y

move :: Board -> Move -> ((Coord, Tile), FacingDir) -> ((Coord, Tile), FacingDir)
move _ TurnLeft (p, dir) = (p, (turnRight . turnRight . turnRight) dir)
move _ TurnRight (p, dir) = (p, turnRight dir)
move board (Forward n) (p, dir) =
  let stuffAheadIncludingMe = dropWhile (/= p) $ cycle $ rowOrCol p dir board
      newCoords = take (n+1) $ takeWhile ((/= Wall) . snd) stuffAheadIncludingMe
  in
    if null newCoords -- can this happen if I'm in it? oh well, already wrote the code
    then (p, dir)
    else (last newCoords, dir)

turnRight :: FacingDir -> FacingDir
turnRight Up = Righ
turnRight Righ = Down
turnRight Down = Lef
turnRight Lef = Up

answerValFor :: FacingDir -> Int
answerValFor Up = 3
answerValFor Righ = 0
answerValFor Down = 1
answerValFor Lef = 2

doPart1 :: String -> String -> Int
doPart1 boardLayout password =
  let allLines = lines boardLayout
      boardRows = zipWith parseLine [1..] allLines :: Board
      path = parsePath $ strip password
      initPos = (head $ head boardRows, Righ)
      allPositions = scanl (flip (move boardRows)) initPos path
      (((column, row),_), facing) = last allPositions
  in 1000 * row + 4 * column + answerValFor facing

doPart2 :: String -> String -> Int
doPart2 boardLayout password =
  let allLines = lines boardLayout
      boardRows = zipWith parseLine [1..] allLines :: Board
      edgeLength = minimum $ map length boardRows
      _path = parsePath $ strip password
  in trace (show edgeLength) 0


parseLine :: Int -> String -> [(Coord, Tile)]
parseLine y input =
  let points = zipWith (\x c -> ((x,y), parseTile c)) [1..] input
  in realOnesOnly points

parseTile :: Char -> Maybe Tile
parseTile ' ' = Nothing
parseTile '.' = Just Open
parseTile '#' = Just Wall
parseTile anythingElse = error (anythingElse : " not expected in board map")

-- not to be confused with imaginary ones such as i
realOnesOnly :: [(a, Maybe b)] -> [(a,b)]
realOnesOnly candidates =
  mapSnd fromJust $ filter (isJust . snd) candidates

parsePath :: String -> [Move]
parsePath [] = []
parsePath ('L':rest) = TurnLeft : parsePath rest
parsePath ('R':rest) = TurnRight : parsePath rest
parsePath input@(a:_)
  | isDigit a = let (num, rest) = span isDigit input in Forward (read num) : parsePath rest
  | otherwise = error (a : " not allowed in password")  -- basically same as a terrible web form
