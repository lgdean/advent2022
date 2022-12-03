module Day03Spec where
{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import Day03

spec :: Spec
spec = do
  describe "Part 1" $ do
    it "can handle given example" $ do
      input <- readFile "inputs/day03-example"
      doPart1 input `shouldBe` 157

    it "can solve Part 1" $ do
      input <- readFile "inputs/day03"
      doPart1 input `shouldBe` 7597

  describe "Part 2" $ do
    it "can handle given example" $ do
      pending
      input <- readFile "inputs/day03-example"
      doPart2 input `shouldBe` 0

    it "can solve Part 2" $ do
      pending
      input <- readFile "inputs/day03"
      doPart2 input `shouldBe` 0
