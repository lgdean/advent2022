module Day12
    (
      doPart1,
      doPart2
    ) where


import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (maybeToList)
import Data.Set ()
import qualified Data.Set as Set

type Square = ((Int, Int), Char)

canStep :: Square -> Square -> Bool
canStep (_, srcH) (_, destH) =
  destH <= succ srcH

neighbors :: (Int, Int) -> Map (Int, Int) a -> [((Int, Int), a)]
neighbors (x,y) grid =
  let coords = neighborCoords (x,y)
  in concatMap (\c -> maybeToList (lookupWithKey c grid)) coords

lookupWithKey :: Ord k => k -> Map k a -> Maybe (k, a)
lookupWithKey key theMap =
  case Map.lookup key theMap of
    Nothing -> Nothing
    Just val -> Just (key, val)

neighborCoords :: (Int, Int) -> [(Int, Int)]
neighborCoords (x,y) = [ (x+1,y), (x-1,y), (x,y+1), (x,y-1) ]

allOneStepTo :: Map (Int, Int) Char -> Square -> [Square]
allOneStepTo grid latest =
  filter (`canStep` latest) $ neighbors (fst latest) grid

searchUntil :: Square -> Map (Int, Int) Char -> Map Square Int -> Int -> [Square] -> Int
searchUntil dest grid shortestTo currN pathsSoFar =
  let candidates = Set.fromList $ concatMap (allOneStepTo grid) pathsSoFar
      notCycling = Set.filter (\p -> Map.findWithDefault 999999 p shortestTo > currN) candidates
      reachedDest = any (\p -> snd p == snd dest) notCycling -- Part 2
--      reachedDest = any (\p -> p == dest) notCycling -- Part 1
  in if reachedDest
     then currN
     else searchUntil dest grid (foldl (\m k -> Map.insert k currN m) shortestTo notCycling) (succ currN) $ Set.toList notCycling

doPart1 :: String -> Int
doPart1 input =
  let grid = parseGrid input
      startCoord = head $ Map.keys $ Map.filter (== 'S') grid
      endCoord = head $ Map.keys $ Map.filter (== 'E') grid
      endOfAllPaths = (endCoord, 'z')
      startOfGoodPaths = (startCoord, 'a')
      heightGrid = Map.map comparableHeightsOnly grid
      best = searchUntil startOfGoodPaths heightGrid Map.empty 1 [endOfAllPaths]
  in best

doPart2 :: String -> Int
doPart2 _ = 0

comparableHeightsOnly :: Char -> Char
comparableHeightsOnly 'S' = 'a'
comparableHeightsOnly 'E' = 'z'
comparableHeightsOnly  x  =  x

parseGrid :: String -> Map (Int, Int) Char
parseGrid input =
  let rows = lines input
  in Map.unions $ zipWith parseRow [0..] rows

parseRow :: Int -> String -> Map (Int, Int) Char
parseRow n row =
  let coords = zip [0..] (repeat n)
  in Map.fromList $ zip coords row
