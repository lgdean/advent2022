module Day19
    (
      doPart1,
      doPart2
    ) where

import Data.List.Split (splitOn)
import Data.Map (Map, (!))
import qualified Data.Map.Strict as Map
import Data.Tuple (swap)

import Debug.Trace (trace)

data Resource = Ore | Clay | Obsidian | Geode deriving (Eq, Ord, Show)
allResources = [Ore, Clay, Obsidian, Geode]
type Cost = [(Int, Resource)]
type Blueprint = Map Resource Cost
type Materials = Map Resource Int
gotNothing :: Materials
gotNothing = Map.fromList $ zip allResources (repeat 0)

type Robots = Map Resource Int -- OK this is asking for trouble
justOneOreRobot :: Robots
justOneOreRobot = Map.fromList $ (Ore,1) : zip [Clay, Obsidian, Geode] (repeat 0)

canAffordRobot :: Blueprint -> Resource -> Materials -> Bool
canAffordRobot blueprint resource availableMaterial =
  let canAffordPart (howMany, r) = howMany < (availableMaterial ! r)
  in all canAffordPart (blueprint ! resource)

waysToBuildRobot :: Blueprint -> Materials -> [(Resource, Materials)]
waysToBuildRobot blueprint availableMaterial =
  let allRobotKinds = allResources
      affordableRobots = filter (\r -> canAffordRobot blueprint r availableMaterial) allRobotKinds :: [Resource]
  in map (\r -> (r, Map.unionWith (-) availableMaterial (Map.fromList (map swap (blueprint ! r))))) affordableRobots



doPart1 :: String -> Int
doPart1 input =
  let allLines = lines input
      blueprints = map parseLine allLines
  in trace (show $ head blueprints) $ length blueprints

doPart2 :: String -> Int
doPart2 input =
  let _allLines = lines input
  in 0


parseLine :: String -> Blueprint
parseLine input =
  let parts = splitOn ":" input
      costParts = take 4 $ splitOn "." $ head $ tail parts -- avoid empty part after last period
      result = map parseCostPart costParts :: [(Resource, Cost)]
  in Map.fromList result

parseCostPart :: String -> (Resource, Cost)
parseCostPart input =
  let parts = tail $ words input
      robotKind = parseResource $ head parts
      costs = parseCosts $ drop 3 parts
  in (robotKind, costs)

parseCosts :: [String] -> Cost
parseCosts [] = []
parseCosts [x] = error ("unexpected single word parsing costs: " ++ x)
parseCosts ("and":more) = parseCosts more
parseCosts (a:b:rest) = (read a, parseResource b) : parseCosts rest

parseResource :: String -> Resource
parseResource "ore" = Ore
parseResource "obsidian" = Obsidian
parseResource "clay" = Clay
parseResource "geode" = Geode
parseResource anythingElse = error ("cannot parse "++anythingElse)
