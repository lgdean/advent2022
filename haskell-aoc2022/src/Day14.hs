module Day14
    (
      doPart1,
      doPart2
    ) where

import Data.List.Split (splitOn)
import Data.Map (Map, member)
import qualified Data.Map.Strict as Map

type Tile = (Int, Int)
type BlockMap = Map Tile Bool

blockPath :: BlockMap -> [Tile] -> BlockMap
blockPath area [] = area  -- or error tbh
blockPath area [_] = area
blockPath area (a:b:rest) = blockPath (blockLine area (a,b)) (b:rest)

blockLine :: BlockMap -> (Tile, Tile) -> BlockMap
blockLine area ((startX, startY), (endX, endY))
  | startX == endX && startY < endY = updateMap area (zip (repeat startX) [startY..endY])
  | startX == endX && startY > endY = updateMap area (zip (repeat startX) [endY..startY])
  | startX < endX && startY == endY = updateMap area [(x,startY) | x <- [startX..endX]]
  | startX > endX && startY == endY = updateMap area [(x,startY) | x <- [endX..startX]]
  | startX == endX && startY == endY = error "did not expect this case; is it real or an error?"
  | otherwise = error "did not expect this case either; is it real or an error?"

-- ok maybe I could have just started with a Set, since these are all boolean
updateMap :: BlockMap -> [Tile] -> BlockMap
updateMap area tiles =
  Map.union area $ Map.fromList $ zip tiles (repeat True)

comeToRest :: Tile -> Int -> BlockMap -> Maybe Tile
comeToRest currPos@(x,y) maxY blockMap =
  let downBlocked = (x, y+1) `member` blockMap
      downLeftBlocked = (x-1, y+1) `member` blockMap
      downRightBlocked = (x+1, y+1) `member` blockMap
  in if y > maxY
     then Nothing
     else if downBlocked
       then if downLeftBlocked
          then if downRightBlocked
               then Just currPos
               else comeToRest (x+1, y+1) maxY blockMap
          else comeToRest (x-1, y+1) maxY blockMap
       else comeToRest (x, y+1) maxY blockMap

sandFall :: Tile -> Int -> BlockMap -> BlockMap
sandFall startPos maxY blockMap =
  case comeToRest startPos maxY blockMap of
    Nothing -> blockMap
    Just landingPos -> sandFall startPos maxY $ Map.insert landingPos True blockMap

doPart1 :: String -> Int
doPart1 input =
  let allLines = lines input
      allPaths = map parsePath allLines
      blockMap = foldl blockPath Map.empty allPaths
      maxY = maximum $ map snd $ Map.keys blockMap
      endMap = sandFall (500,0) maxY blockMap
  in Map.size endMap - Map.size blockMap

doPart2 :: String -> Int
doPart2 _ =
  0

parsePath :: String -> [Tile]
parsePath input =
  let parts = splitOn " -> " input
  in map parseTile parts

parseTile :: String -> Tile
parseTile input =
  case splitOn "," input of
    [x,y] -> (read x, read y)
    _     -> error ("cannot parse tile: "++input)
