module Day08
    (
      doPart1,
      doPart2
    ) where

import Data.Char (digitToInt)
import Data.List (findIndex, tails, transpose)
import Data.Map (unionsWith)
import qualified Data.Map.Strict as Map
import Data.Set ()
import qualified Data.Set as Set

allVisibleOver :: Int -> [(Int, (Int, Int))] -> [(Int, Int)]
allVisibleOver _ [] = []
allVisibleOver 9 _ = []
allVisibleOver prevHeight ((height, loc):rest)
  | prevHeight < height = loc : allVisibleOver height rest
  | otherwise           = allVisibleOver prevHeight rest

doPart1 :: String -> Int
doPart1 input =
  let grid = parseLayout input
      visibleLeft = map (allVisibleOver (-1)) grid
      visibleRight = map (allVisibleOver (-1)) (map reverse grid)
      visibleTop = map (allVisibleOver (-1)) (transpose grid)
      visibleBottom = map (allVisibleOver (-1)) (map reverse $ transpose grid)
      allVisible = Set.unions $ map Set.fromList $ concat [visibleLeft, visibleRight, visibleTop, visibleBottom]
  in Set.size allVisible

-- messy
nVisibleBeyond :: [(Int, (Int, Int))] -> ((Int, Int), Int)
nVisibleBeyond [] = ((99999,99999), 0)
nVisibleBeyond ((h,c):rest) =
  answer
--  where answer = (c, length $ takeWhile (\(otherH, _ ) -> otherH < h) rest)
  where
    blockingTreeIndex = findIndex (\(otherH,_) -> h<=otherH) rest
    answer = (c, case blockingTreeIndex of
                   Just n -> n+1
                   Nothing -> length rest)

doPart2 :: String -> Int
doPart2 input =
  let grid = parseLayout input
      flipped = map reverse grid
      transposed = transpose grid
      bothWays = map reverse $ transpose grid
      resultFor someGrid = concatMap (map nVisibleBeyond . tails) someGrid
      results = map resultFor [grid, flipped, transposed, bothWays]
      scores = unionsWith (*) $ map Map.fromList results
  in maximum $ Map.elems scores

-- WARNING x and y are not as expected here! fix later.
parseLayout :: String -> [[(Int, (Int, Int))]]
parseLayout input =
  let rows = lines input
  in zipWith parseRow [0..] rows

parseRow :: Int -> String -> [(Int, (Int, Int))]
parseRow n row =
  let coords = zip [0..] (repeat n)
  in zip (map digitToInt row) coords
