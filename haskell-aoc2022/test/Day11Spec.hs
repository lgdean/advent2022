module Day11Spec where
{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import Day11
import Lib

spec :: Spec
spec = do
  describe "Part 1" $ do
    it "can handle given example" $ do
      input <- readFile "inputs/day11-example"
      let monkeys = [
                      Monkey {name=0, op=(*) 19, test = (`divisibleBy` 23), testResults = (2,3)}
                    , Monkey {name=1, op=(+) 6, test = (`divisibleBy` 19), testResults = (2,0)}
                    , Monkey {name=2, op=(\x -> x*x), test = (`divisibleBy` 13), testResults = (1,3)}
                    , Monkey {name=3, op=(+) 3, test = (`divisibleBy` 17), testResults = (0,1)}
                    ]
      let startItems = [ [79, 98]
                       , [54, 65, 75, 74]
                       , [79, 60, 97]
                       , [74]
                       ]
      doPart1 monkeys startItems `shouldBe` 10605

    it "can solve Part 1" $ do
      input <- readFile "inputs/day11"
      let monkeys = [
                      Monkey {name=0, op=(*) 13, test = (`divisibleBy` 11), testResults = (4,7)}
                    , Monkey {name=1, op=(+) 4, test = (`divisibleBy` 17), testResults = (2,6)}
                    , Monkey {name=2, op=(*) 11, test = (`divisibleBy` 5), testResults = (6,5)}
                    , Monkey {name=3, op=(+) 8, test = (`divisibleBy` 13), testResults = (1,2)}
                    , Monkey {name=4, op=(\x -> x*x), test = (`divisibleBy` 19), testResults = (3,1)}
                    , Monkey {name=5, op=(+) 5, test = (`divisibleBy` 2), testResults = (7,0)}
                    , Monkey {name=6, op=(+) 1, test = (`divisibleBy` 3), testResults = (0,5)}
                    , Monkey {name=7, op=(+) 3, test = (`divisibleBy` 7), testResults = (4,3)}
                    ]
      let startItems = [ [98, 97, 98, 55, 56, 72]
                       , [73, 99, 55, 54, 88, 50, 55]
                       , [67, 98]
                       , [82, 91, 92, 53, 99]
                       , [52, 62, 94, 96, 52, 87, 53, 60]
                       , [94, 80, 84, 79]
                       , [89]
                       , [70, 59, 63]
                       ]
      doPart1 monkeys startItems `shouldBe` 54036

  describe "Part 2" $ do
    it "can handle given example" $ do
      pending
      input <- readFile "inputs/day11-example"
      doPart2 input `shouldBe` undefined

    it "can solve Part 2" $ do
      pending
      input <- readFile "inputs/day11"
      doPart2 input `shouldBe` 0
