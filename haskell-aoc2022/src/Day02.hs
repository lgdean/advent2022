module Day02
    (
      doPart1,
      doPart2
    ) where

-- Is this better than hard-coding all 9 possibilities for each Part? IDK.

data Play = Rock | Paper | Scissors deriving Eq

scoreRound :: (Play, Play) -> Int
scoreRound (opp, mine) = scoreShape mine + scoreResult (opp, mine)

scoreShape :: Play -> Int
scoreShape Rock = 1
scoreShape Paper = 2
scoreShape Scissors = 3

scoreResult :: (Play, Play) -> Int
scoreResult (opp, mine)
  | opp == mine = 3
  | opp == shapeToWinOver mine = 0
  | mine == shapeToWinOver opp = 6
  | otherwise = error "fix yer code"

shapeToWinOver :: Play -> Play
shapeToWinOver Rock = Paper
shapeToWinOver Paper = Scissors
shapeToWinOver Scissors = Rock

doPart1 :: [Char] -> Int
doPart1 input =
  let allLines = lines input
      pairs = map parseLine allLines
  in sum $ map scoreRound pairs

doPart2 :: [Char] -> Int
doPart2 input =
  let allLines = lines input
      pairs = map parseLinePart2 allLines
  in sum $ map scoreRound pairs

parseLinePart2 :: String -> (Play, Play)
parseLinePart2 line =
  case words line of
    [] -> error "no words in line!"
    [a,"Y"] -> (parsePlay a, parsePlay a)
    [a,"Z"] -> (parsePlay a, shapeToWinOver (parsePlay a))
    [a,"X"] -> (parsePlay a, shapeToWinOver (shapeToWinOver (parsePlay a)))
    whoops -> error ("line contains surprising parts: " ++ show whoops)

parseLine :: String -> (Play, Play)
parseLine line =
  case words line of
    [] -> error "no words in line!"
    [a,b] -> (parsePlay a, parsePlay b)
    whoops -> error ("line contains surprising parts: " ++ show whoops)

parsePlay :: String -> Play
parsePlay ['A'] = Rock
parsePlay ['B'] = Paper
parsePlay ['C'] = Scissors
parsePlay ['X'] = Rock
parsePlay ['Y'] = Paper
parsePlay ['Z'] = Scissors
parsePlay whoops = error whoops
