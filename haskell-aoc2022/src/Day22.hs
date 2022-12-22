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

doPart1 :: String -> String -> Int
doPart1 boardLayout password =
  let allLines = lines boardLayout
      boardRows = zipWith parseLine [1..] allLines
      path = parsePath $ strip password
  in trace (show $ last path) $ length allLines

doPart2 :: String -> Int
doPart2 input =
  let _allLines = lines input
  in 0


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
