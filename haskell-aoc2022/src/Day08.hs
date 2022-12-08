module Day08
    (
      doPart1,
      doPart2
    ) where

import Data.Char (digitToInt)
import Data.List (transpose)
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

doPart2 :: String -> Int
doPart2 input =
  0

-- WARNING x and y are not as expected here! fix later.
parseLayout :: String -> [[(Int, (Int, Int))]]
parseLayout input =
  let rows = lines input
  in zipWith parseRow [0..] rows

parseRow :: Int -> String -> [(Int, (Int, Int))]
parseRow n row =
  let coords = zip [0..] (repeat n)
  in zip (map digitToInt row) coords
