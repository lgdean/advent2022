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
                     , op :: Int -> Int
                     , test :: Int -> Bool
                     , testResults :: (Int, Int)
                     }
type WorryLevel = Int -- same thing as an item, for now

-- returns every throw that happens in this round; TODO handle all monkeys!
doRound :: [Monkey] -> [[WorryLevel]] -> [((Int, Int), WorryLevel)]
doRound monkeys items =
  let firstTurnThrows = doTurn (monkeys !! 0) (items !! 0) ++ map snd (filter ((== 0) . snd . fst) [])
      secondTurnThrows = doTurn (monkeys !! 1) $ (items !! 1) ++ map snd (filter ((== 1) . snd . fst) firstTurnThrows)
      thirdTurnThrows = doTurn (monkeys !! 2) $ (items !! 2) ++ map snd (filter ((== 2) . snd . fst) (firstTurnThrows++secondTurnThrows))
      fourthTurnThrows = doTurn (monkeys !! 3) $ (items !! 3) ++ map snd (filter ((== 3) . snd . fst) (firstTurnThrows++secondTurnThrows++thirdTurnThrows))
  in firstTurnThrows ++ secondTurnThrows ++ thirdTurnThrows ++ fourthTurnThrows

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
          let divided = op monkey worry `div` 3
              throwTo = if test monkey divided then fst $ testResults monkey else snd $ testResults monkey
          in ((name monkey, throwTo), divided)

doPart1 :: [Monkey] -> [[WorryLevel]] -> Int
doPart1 monkeys startItems =
  let firstRoundResult = doRound monkeys startItems :: [((Int, Int), WorryLevel)]
      unhandledThrows roundResult = filter (not . alreadyHandled) roundResult :: [((Int, Int), WorryLevel)]
      itemsForMonkeys prev = map (\m -> map snd $ filter (thrownTo m) prev) monkeys :: [[WorryLevel]]
      myDoRound = doRound monkeys . itemsForMonkeys . unhandledThrows :: [((Int, Int), WorryLevel)] -> [((Int, Int), WorryLevel)]
      allResults = take 20 $ iterate myDoRound firstRoundResult
      throwers = group $ sort $ map whoThrew $ concat allResults
      howMany = map length throwers
  in product $ take 2 $ reverse $ sort howMany

doPart2 :: String -> Int
doPart2 input =
  0
