module Day13Spec where
{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import Day13

spec :: Spec
spec = do
  describe "Part 1" $ do
    it "can handle given example" $ do
      input <- readFile "inputs/day13-example"
      doPart1 input `shouldBe` 13

    it "can solve Part 1" $ do
      input <- readFile "inputs/day13"
      doPart1 input `shouldBe` 5196

  describe "Part 2" $ do
    it "can handle given example" $ do
      input <- readFile "inputs/day13-example"
      doPart2 input `shouldBe` 140

    it "can solve Part 2" $ do
      input <- readFile "inputs/day13"
      doPart2 input `shouldBe` 22134
