module Day01Spec where
{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import Day01

spec :: Spec
spec = do
  describe "Part 1" $ do
    it "can handle given example" $ do
      input <- readFile "inputs/day01-example"
      doPart1 input `shouldBe` 24000

    it "can solve Part 1" $ do
      input <- readFile "inputs/day01"
      doPart1 input `shouldBe` 71300

  describe "Part 2" $ do
    it "can handle given example" $ do
      input <- readFile "inputs/day01-example"
      doPart2 input `shouldBe` 45000

    it "can solve Part 2" $ do
      input <- readFile "inputs/day01"
      doPart2 input `shouldBe` 0
