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
      doPart1 input `shouldBe` 3068

    it "can solve Part 1" $ do
      input <- readFile "inputs/day17"
      doPart1 input `shouldBe` 3200

  describe "Part 2" $ do
    it "can handle given example" $ do
      pending
      input <- readFile "inputs/day17-example"
      doPart2 input `shouldBe` undefined

    it "can solve Part 2" $ do
      pending
      input <- readFile "inputs/day17"
      doPart2 input `shouldBe` 0
