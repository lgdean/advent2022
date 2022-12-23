module Day23Spec where
{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import Day23

spec :: Spec
spec = do
  describe "Part 1" $ do
    it "can handle given example" $ do
      input <- readFile "inputs/day23-example"
      doPart1 input `shouldBe` 110

    it "can solve Part 1" $ do
      input <- readFile "inputs/day23"
      doPart1 input `shouldBe` 4114

  describe "Part 2" $ do
    it "can handle given example" $ do
      input <- readFile "inputs/day23-example"
      doPart2 input `shouldBe` 20

    it "can solve Part 2" $ do
      input <- readFile "inputs/day23"
      doPart2 input `shouldBe` 970
