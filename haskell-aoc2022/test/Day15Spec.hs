module Day15Spec where
{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import Day15

spec :: Spec
spec = do
  describe "Part 1" $ do
    it "can handle given example" $ do
      input <- readFile "inputs/day15-example"
      doPart1 10 input `shouldBe` 26

    it "can solve Part 1" $ do
      input <- readFile "inputs/day15"
      doPart1 2000000 input `shouldBe` 4811413

  describe "Part 2" $ do
    it "can handle given example" $ do
      input <- readFile "inputs/day15-example"
      doPart2 20 input `shouldBe` 56000011

    it "can solve Part 2" $ do
      input <- readFile "inputs/day15"
      doPart2 4000000 input `shouldBe` 13171855019123
