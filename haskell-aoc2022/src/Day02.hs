module Day02
    (
      doPart1,
      doPart2
    ) where

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
  | beats opp mine = 0
  | beats mine opp = 6
  | otherwise = error "fix yer code"

beats :: Play -> Play -> Bool
beats Paper Rock = True
beats Rock Scissors = True
beats Scissors Paper = True
beats _ _ = False

doPart1 :: [Char] -> Int
doPart1 input =
  let allLines = lines input
      pairs = map parseLine allLines
  in sum $ map scoreRound pairs

doPart2 :: [Char] -> Int
doPart2 input =
  0

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
