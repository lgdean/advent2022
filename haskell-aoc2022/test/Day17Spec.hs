module Day17Spec where
{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import Day17

spec :: Spec
spec = do
  describe "Part 1" $ do
    it "can detect walls" $ do
      hitsRight Dash (4,0) `shouldBe` True
      hitsRight Dash (3,0) `shouldBe` False
      hitsRight Plus (3,6) `shouldBe` False
      -- these turned out not to be the issue, so enough of that

    it "can handle given example" $ do
      input <- readFile "inputs/day17-example"
      doPart1 input `shouldBe` 3068 -- 2022

    it "can solve Part 1" $ do
      pending
      input <- readFile "inputs/day17"
      doPart1 input `shouldBe` 3200

  describe "Part 2" $ do
    it "can handle given example" $ do
      pending
      input <- readFile "inputs/day17-example"
      doPart2 1000 input `shouldBe` 1520
--      doPart2 1500 input `shouldBe` 2277
--      doPart2 1956 input `shouldBe` 2966
--      doPart2 1978 input `shouldBe` 3000
--      doPart2 2000 input `shouldBe` 3034
--      doPart2 2022 input `shouldBe` 3068
--      doPart2 2500 input `shouldBe` 3788

    it "can solve Part 2" $ do
      pending
      input <- readFile "inputs/day17"
      doPart2 1000 input `shouldBe` 1576
--      doPart2 1500 input `shouldBe` 2375
--      doPart2 2000 input `shouldBe` 3165
--      doPart2 2022 input `shouldBe` 3200
--      doPart2 2500 input `shouldBe` 3960
--      doPart2 5000 input `shouldBe` 7931
--      doPart2 10000 input `shouldBe` 15842 -- verified in slow way too
--      doPart2 100000 input `shouldBe` 158499 -- probably: used take 200 prevRocks
