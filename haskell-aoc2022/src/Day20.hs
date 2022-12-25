module Day20
    (
      doPart1,
      doPart2,
      mixFile,
      readAndMixFile
    ) where

-- may not be the right representation, but let's start there
mixFile :: Int -> [Int] -> [Int]
mixFile howManyTimes nums =
  let initState = zip nums [0..]
      toProcess = length nums
  in map fst $ mixRemaining 0 toProcess toProcess initState

mixRemaining :: Int -> Int-> Int -> [(Int, Int)] -> [(Int, Int)]
mixRemaining _ 0 _ theList =
  theList
mixRemaining currIndex howManyMore totalSize theList =
  let (after, (curr,_):before) = break ((==currIndex) . snd) theList
      (_, theHeadIndex) = head theList
      (_, theSecondIndex) = head $ tail theList
      almostNewList = before ++ after
      howFarMove = curr `mod` (totalSize - 1)
      (newBefore, newAfter) = splitAt howFarMove almostNewList
      newList = newBefore ++ (curr, currIndex) : newAfter
      newHeadIndex = if theHeadIndex == currIndex && howFarMove /= 0 then theSecondIndex else theHeadIndex
      newListAdjusted = take totalSize $ dropWhile ((/= newHeadIndex) . snd) $ cycle newList
  in mixRemaining (currIndex+1) (howManyMore-1) totalSize newListAdjusted

readAndMixFile :: String -> [Int]
readAndMixFile input =
  let allLines = lines input
      numbers = map read allLines :: [Int]
  in mixFile 1 numbers

answerFrom :: [Int] -> Int
answerFrom mixedFile =
  let indices = [1000, 2000, 3000]
      cycledMix = dropWhile (/= 0) $ cycle mixedFile
      values = map (cycledMix !!) indices
  in sum values

doPart1 :: String -> Int
doPart1 input =
  let allLines = lines input
      numbers = map read allLines :: [Int]
      firstMix = mixFile 1 numbers
  in answerFrom firstMix

doPart2 :: String -> Int
doPart2 input =
  let _allLines = lines input
  in 0
