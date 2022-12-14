module Day15
    (
      doPart1,
      doPart2
    ) where

import Data.Char (isDigit)
import Data.List.Split (splitOn)
import Data.Range (Range(SingletonRange), (+=+), (*=*), difference, fromRanges, intersection, invert, mergeRanges)

type Tile = (Int, Int)

manhattanDistance :: Tile -> Tile -> Int
manhattanDistance (x1, y1) (x2, y2) =
  abs (x1-x2) + abs (y1 -y2)

ruledOutBy :: Int -> Tile -> Int -> Range Int
ruledOutBy rowY (sensorX, sensorY) maxDist =
  let diffY = abs (rowY - sensorY)
      diffX = maxDist - diffY
  in
    if diffX < 0
    then sensorX *=* sensorX -- empty Range
    else (sensorX - diffX) +=+ (sensorX + diffX)

doPart1 :: Int -> String -> Int
doPart1 rowY input =
  let allLines = lines input
      allInfos = map parseLine allLines
      sensorsWithDist = map (\(s,b) -> (s, manhattanDistance s b)) allInfos
      ruledOut = map (uncurry (ruledOutBy rowY)) sensorsWithDist
      actualBeaconXs = map (fst . snd) $ filter (\(_, (_,y)) -> y==rowY) allInfos
      countThese = difference ruledOut $ map SingletonRange actualBeaconXs
  in length $ fromRanges countThese

doPart2 :: Int -> String -> Int
doPart2 maxCoord input =
  let allLines = lines input
      allInfos = map parseLine allLines
      sensorsWithDist = map (\(s,b) -> (s, manhattanDistance s b)) allInfos
      ruledOut rowY = map (uncurry (ruledOutBy rowY)) sensorsWithDist
      notRuledOut rowY = intersection [0 +=+ maxCoord] $ invert $ ruledOut rowY :: [Range Int]
      candidates = filter (not . null . fst) $ map (\y -> (fromRanges $ notRuledOut y, y)) [0..maxCoord]
      (x,y) = head candidates
  in 4000000 * head x + y


parseLine :: String -> (Tile, Tile)
parseLine input =
  let parts = splitOn ":" input
      sensorInfo = map justTheNumber $ splitOn "," (head parts)
      beaconInfo = map justTheNumber $ splitOn "," (parts !! 1)
  in ((head sensorInfo,  sensorInfo !! 1),
      (head beaconInfo,  beaconInfo !! 1))

justTheNumber :: String -> Int
justTheNumber inputPart =
  let relevantChar c = isDigit c || '-' == c
  in read $ filter relevantChar inputPart


-- When the range is empty, it throws instead of returning an empty list.
-- I gave up on trying to get it to work.

-- import Control.Exception (evaluate, try)

--tryExceptionFromRanges :: [Range Int] -> [Int]
--tryExceptionFromRanges xs = do
--  eitherException <- try (evaluate (fromRanges (mergeRanges xs)))
--  case eitherException of
--    Left _       -> []
--    Right answer -> answer
