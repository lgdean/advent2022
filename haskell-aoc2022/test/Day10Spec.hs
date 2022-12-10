module Day10Spec where
{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import Day10

spec :: Spec
spec = do
  describe "Part 1" $ do
    it "can handle given example" $ do
      input <- readFile "inputs/day10-example"
      doPart1 input `shouldBe` 13140

    it "can solve Part 1" $ do
      input <- readFile "inputs/day10" -- not 15080
      doPart1 input `shouldBe` 14620

  describe "Part 2" $ do
    it "can handle given example" $ do
      pending
      input <- readFile "inputs/day10-example"
      doPart2 input `shouldBe` undefined

    it "can solve Part 2" $ do
      pending
      input <- readFile "inputs/day10"
      doPart2 input `shouldBe` 0
