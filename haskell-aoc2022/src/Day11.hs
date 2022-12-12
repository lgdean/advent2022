module Day11
    (
      doPart1,
      doPart2,
      Monkey (..)
    ) where

import Data.List (group, sort)
import Data.Set ()

import Debug.Trace (trace)

-- the puzzle input has 36 total items, so lists may work fine for the queues

data Monkey = Monkey { name :: Int
                     , op :: WorryLevel -> WorryLevel
                     , test :: WorryLevel -> Bool
                     , testResults :: (Int, Int)
                     }
type WorryLevel = Integer -- same thing as an item, for now

-- returns every throw that happens in this round
doRound :: [Monkey] -> [[WorryLevel]] -> [((Int, Int), WorryLevel)]
doRound monkeys items =
  let doNextTurn throwsSoFar (monkey, startItems)
        = throwsSoFar ++ doTurn monkey (startItems ++ map snd (filter (thrownTo monkey) throwsSoFar))
  in foldl doNextTurn [] (zip monkeys items)

thrownTo :: Monkey -> ((Int, Int), WorryLevel) -> Bool
thrownTo Monkey {name=n} ((_, to), _) = n == to

thrownBy :: Monkey -> ((Int, Int), WorryLevel) -> Bool
thrownBy Monkey {name=n} ((from, _), _) = n == from

whoThrew :: ((Int, Int), WorryLevel) -> Int
whoThrew ((from, _), _) = from

-- receiving Monkey handled the item in the same round, after it was thrown
alreadyHandled :: ((Int, Int), WorryLevel) -> Bool
alreadyHandled ((from, to), _) = from < to

doTurn :: Monkey -> [WorryLevel] -> [((Int, Int), WorryLevel)]
doTurn monkey =
  map inspect
  where inspect worry =
          let operated = op monkey worry
              divided = operated `div` 3
              throwTo = if test monkey divided then fst $ testResults monkey else snd $ testResults monkey
          in ((name monkey, throwTo), divided)

doPart1 :: [Monkey] -> [[WorryLevel]] -> Integer
doPart1 monkeys startItems =
  let firstRoundResult = doRound monkeys startItems :: [((Int, Int), WorryLevel)]
      unhandledThrows roundResult = filter (not . alreadyHandled) roundResult :: [((Int, Int), WorryLevel)]
      itemsForMonkeys prev = map (\m -> map snd $ filter (thrownTo m) prev) monkeys :: [[WorryLevel]]
      myDoRound = doRound monkeys . itemsForMonkeys . unhandledThrows :: [((Int, Int), WorryLevel)] -> [((Int, Int), WorryLevel)]
      allResults = take 20 $ iterate myDoRound firstRoundResult
      throwers = group $ sort $ map whoThrew $ concat allResults
      howMany = map (fromIntegral . length) throwers
  in product $ take 2 $ reverse $ sort howMany

doPart2 :: String -> Int
doPart2 input =
  0
