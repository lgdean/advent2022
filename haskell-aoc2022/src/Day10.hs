module Day10
    (
      doPart1,
      doPart2
    ) where

import Data.List (intercalate)
import Data.List.Split (chunksOf)

import Debug.Trace (trace)

data Instruction = Noop | AddX Int

runProgram :: Int -> [Instruction] -> [Int]
runProgram _ [] = []
runProgram currX (Noop:rest) = currX : runProgram currX rest
runProgram currX (AddX v : rest) = currX : (currX + v) : runProgram (currX + v) rest

doPart1 :: String -> Int
doPart1 input =
  let allLines = lines input
      program = map parseLine allLines
      resultStream = 1 : runProgram 1 program
      -- this line started out simple when I first wrote it (incorrectly)
      everyTwentySignals = map last $ filter ((20 ==) . length) $ chunksOf 20 $ zip [1..] resultStream
      interestingSignals = map head $ chunksOf 2 everyTwentySignals
  in sum $ map (uncurry (*)) interestingSignals

doPart2 :: String -> String
doPart2 input =
  let allLines = lines input
      program = map parseLine allLines
      resultStream = 1 : runProgram 1 program
      isVisible pos regX = abs (pos-regX) <= 1
      crtRows = map (zipWith isVisible [0..]) $ chunksOf 40 resultStream
      renderRow = map (\b -> if b then '#' else '.')
      result = intercalate "\n" $ map renderRow crtRows
--  in trace result result
  in result

parseLine :: String -> Instruction
parseLine line =
  case words line of
    ["noop"] -> Noop
    ["addx", v] -> AddX (read v)
    _        -> error ("failed to parse line: " ++ line)
