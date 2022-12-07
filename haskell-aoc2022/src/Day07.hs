module Day07
    (
      doPart1,
      doPart2
    ) where

import Data.List (isPrefixOf, nub)
import Data.Map (Map, filterWithKey, findWithDefault, insertWith, keys, mapKeys)
import qualified Data.Map.Strict as Map

-- ok, if a file knows its own name then maybe a dir should too? oh well.
type File = (String, Int)
data Dir = EmptyOrUnknown
         | ListedDir [(String, Dir)] [File]

type DirPath = [String]
type ListingResult = Map DirPath [File]

dirFromListing :: ListingResult -> Dir
dirFromListing result =
  let files = findWithDefault [] [] result
      dirNames = nub $ map head $ filter (not . null) $ keys result :: [String]
      subTree dirName = mapKeys tail $ filterWithKey (\k _ -> [dirName] `isPrefixOf` k) result
  in ListedDir (map (\x -> (x, dirFromListing $ subTree x)) dirNames) files

dirSize :: Dir -> Int
dirSize EmptyOrUnknown = 0
dirSize (ListedDir dirs files) =
  sum (map (dirSize . snd) dirs) + sum (map snd files)

-- none of this will be particularly efficient
findSubDirsWhere :: (Dir -> Bool) -> Dir -> [Dir]
findSubDirsWhere _ EmptyOrUnknown = []
findSubDirsWhere p dir@(ListedDir subs _) =
  if p dir then dir : mapSubDirs else mapSubDirs
  where mapSubDirs = concatMap (findSubDirsWhere p . snd) subs :: [Dir]

doPart1 :: [Char] -> Int
doPart1 input =
  let listing = parseListings [] Map.empty $ lines input
      rootDir = dirFromListing listing
      relevantDirs = findSubDirsWhere (\d -> dirSize d <= 100000) rootDir
  in sum $ map dirSize relevantDirs

doPart2 :: [Char] -> Int
doPart2 input =
  let listing = parseListings [] Map.empty $ lines input
      rootDir = dirFromListing listing
      totalDiskSpace = 70000000
      totalUsed = dirSize rootDir
      currentFree = totalDiskSpace - totalUsed
      needToFreeUp = 30000000 - currentFree
      relevantDirs = findSubDirsWhere (\d -> dirSize d >= needToFreeUp) rootDir
  in minimum (map dirSize relevantDirs)

-- let's assume we never cd into a dir that does not exist
parseListings :: DirPath -> ListingResult -> [String] -> ListingResult
parseListings _ resultSoFar [] = resultSoFar
parseListings pathReversed resultSoFar (line:rest) =
  case words line of
    ["$", "cd", "/"] -> parseListings [] resultSoFar rest
    ["$", "cd", ".."] -> parseListings (tail pathReversed) resultSoFar rest
    ["$", "cd", d] -> parseListings (d:pathReversed) resultSoFar rest
    ["$", "ls"] -> parseListings pathReversed resultSoFar rest
    ["dir", _] -> parseListings pathReversed resultSoFar rest
    [sz, name] -> parseListings pathReversed
                                (insertWith (++) (reverse pathReversed) [(name, read sz)] resultSoFar)
                                rest
    _ -> error ("parse error on: " ++ line)
