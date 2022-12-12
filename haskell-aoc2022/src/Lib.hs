module Lib
    ( readIntLines
    , parseChunks
    , fixedPoint
    , bin2Int
    , divisibleBy
    ) where

import Data.Char(digitToInt)
import Data.List.Split (splitOn)

readIntLines :: [Char] -> [Int]
readIntLines = map read . lines

-- parse chunks of an input file, separated by empty lines
parseChunks :: ([String] -> a) -> String -> [a]
parseChunks chunkParser input =
  let allLines = lines input
      chunks = splitOn [""] allLines
  in map chunkParser chunks

fixedPoint :: Eq a => (a -> a) -> a -> a
fixedPoint f initState =
  let nextState = f initState
  in if initState == nextState then nextState else fixedPoint f nextState

bin2Int :: String -> Int
bin2Int str = foldl (\acc n -> acc*2+n) 0 (map digitToInt str)

divisibleBy :: Integral a => a -> a -> Bool
divisibleBy candidate other = candidate `mod` other == 0
