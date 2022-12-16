module Day16
    (
      doPart1,
      doPart2
    ) where

import Data.Char (isAlpha, isDigit)
import Data.List.Split (splitOn)
import Data.Map (Map, (!))
import qualified Data.Map.Strict as Map

-- we care about the path distance between valves with non-zero flow rates
-- (and from AA to one of them)

moreDistances :: Map String [String] -> Int -> [String] -> Map String Int -> Map String Int
moreDistances neighborMap dist currGen soFar
  | Map.size neighborMap == Map.size soFar = soFar
  | otherwise =
      let nextGenCands = concatMap (neighborMap !) currGen
          nextGen = filter (`Map.notMember` soFar) nextGenCands
          newDistMap = Map.union soFar $ Map.fromList $ zip nextGenCands $ repeat dist
      in moreDistances neighborMap (succ dist) nextGen newDistMap

distancesFrom :: Map String [String] -> String -> Map String Int
distancesFrom neighborMap src =
  moreDistances neighborMap 1 [src] $ Map.fromList [(src,0)]

relevantDistances :: Map String [String] -> [String] -> String -> Map (String,String) Int
relevantDistances neighborMap dests src =
  let allDists = distancesFrom neighborMap src
      relevantOnes = Map.filterWithKey (\k _ -> k `elem` dests && k /= src) allDists
  in Map.mapKeys (\k -> (src,k)) relevantOnes

-- this is not the most efficient way to do this calculation
-- (though the calculation itself is OK)
pathsOfDuration :: Map (String, String) Int -> Map String Int -> Int -> [String] -> Int -> [(Int, [String])]
pathsOfDuration _ _ 0 pathSoFar totalSoFar = [(totalSoFar, pathSoFar)]
pathsOfDuration _ _ _ [] _ = error "can't go somewhere from nowhere"
pathsOfDuration distMap flowMap minsLeft (v:prev) totalSoFar
  | minsLeft < minimum (Map.elems distMap) = [(totalSoFar, v:prev)]
  | otherwise =
      let dests = Map.mapKeys snd $ Map.filterWithKey (\(f,t) _ -> f == v && t `notElem` prev) distMap
          timeLeft (_, dist) = minsLeft - (dist + 1) -- we always open the valve on first visit; if not, that's a different path
          pressureRelease (valve, dist) = if valve `notElem` prev then (flowMap ! valve) * max 0 (timeLeft (valve, dist)) else 0
          pathsFrom dest = pathsOfDuration distMap flowMap (timeLeft dest) (fst dest:v:prev) (pressureRelease dest + totalSoFar)
      in
        if null dests
        then [(totalSoFar, v:prev)]
        else concatMap pathsFrom $ Map.assocs dests

doPart1 :: String -> Int
doPart1 input =
  let allLines = lines input
      allInfos = map parseLine allLines
      flowRates = map (\(n,f,_) -> (n,f)) allInfos
      neighbors = Map.fromList $ map (\(n,_,ns) -> (n,ns)) allInfos
      startValve = "AA"
      openableValves = map fst $ filter ((> 0) . snd) flowRates
      relevantValves = startValve : openableValves
      distanceMap = Map.unions $ map (relevantDistances neighbors relevantValves) relevantValves
      allPaths = pathsOfDuration distanceMap (Map.fromList flowRates) 30 ["AA"] 0
  in maximum $ map fst allPaths

doPart2 :: String -> Int
doPart2 _ = 0


parseLine :: String -> (String, Int, [String])
parseLine input =
  let parts = splitOn ";" input
      (name, flowRate) = parseFirstPart (head parts)
      neighbors = parseTunnels $ head $ tail parts
  in (name, flowRate, neighbors)

parseFirstPart :: String -> (String, Int)
parseFirstPart input =
  let parts = tail $ words input
      valveName = head parts
      flowRate = read $ filter isDigit $ parts !! 3
  in (valveName, flowRate)

parseTunnels :: String -> [String]
parseTunnels input =
  let tunnelParts = drop 4 $ words input
  in map (filter isAlpha) tunnelParts
