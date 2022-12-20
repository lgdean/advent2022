module Day20
    (
      doPart1,
      doPart2,
      readAndMixFile
    ) where

-- may not be the right representation, but let's start there
mixFile :: [Int] -> [Int]
mixFile nums =
  let initState = zip nums [0..]
      toProcess = length nums
  in map fst $ mixRemaining 0 toProcess toProcess initState

mixRemaining :: Int -> Int-> Int -> [(Int, Int)] -> [(Int, Int)]
mixRemaining _ 0 _ theList =
  let (after, before) = break ((==0) . snd) theList
  in before ++ after
mixRemaining currIndex howManyMore totalSize theList =
  let (after, (curr,_):before) = break ((==currIndex) . snd) theList
      almostNewList = before ++ after
      howFarMove = curr `mod` (totalSize - 1)
      (newBefore, newAfter) = splitAt howFarMove almostNewList
      newList = newBefore ++ (curr, currIndex) : newAfter
  in mixRemaining (currIndex+1) (howManyMore-1) totalSize newList

readAndMixFile :: String -> [Int]
readAndMixFile input =
  let allLines = lines input
      numbers = map read allLines :: [Int]
  in mixFile numbers

doPart1 :: String -> Int
doPart1 input =
  let allLines = lines input
      numbers = map read allLines :: [Int]
      firstMix = mixFile numbers
      indices = [1000, 2000, 3000]
      cycledMix = dropWhile (/= 0) $ cycle firstMix
      values = map (cycledMix !!) indices
  in sum values

doPart2 :: String -> Int
doPart2 input =
  let _allLines = lines input
  in 0
