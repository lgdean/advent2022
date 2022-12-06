module Day05
    (
      doPart1,
      doPart2
    ) where

import Data.Map (Map)
import qualified Data.Map.Strict as Map

type Move = (Int, Int, Int)
type CrateArrangement = Map Int [Char]

doMove :: Move -> CrateArrangement -> CrateArrangement
doMove (howMany, src, dest) curr =
  let (toMove, toKeep) = splitAt howMany (curr Map.! src)
  in Map.adjust (addCrates toMove) dest $ Map.insert src toKeep curr

addCrates :: [a] -> [a] -> [a]
--addCrates new old = foldl (flip (:)) old new -- Part 1
addCrates new old = new ++ old

doPart1 :: [(Int, [Char])] -> [Char] -> [Char]
doPart1 startState input =
  let allLines = lines input
      moves = map parseMove allLines
      cratesStart = Map.fromList startState
      cratesEnd = foldl (flip doMove) cratesStart moves
  in map head $ Map.elems cratesEnd

doPart2 :: [(Int, [Char])] -> [Char] -> [Char]
doPart2 = doPart1

parseMove :: String -> Move
parseMove move =
  case words move of
    [_, n, _, src, _, dest] -> (read n, read src, read dest)
    _ -> error ("failed to parse; " ++ move)
