module Day16Spec where
{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import Day16

spec :: Spec
spec = do
  describe "Part 1" $ do
    it "can handle given example" $ do
      input <- readFile "inputs/day16-example"
      doPart1 input `shouldBe` 1651

    it "can solve Part 1" $ do
      input <- readFile "inputs/day16"
      doPart1 input `shouldBe` 1857

  describe "Part 2" $ do
    it "can handle given example" $ do
      pending
      input <- readFile "inputs/day16-example"
      doPart2 input `shouldBe` 1707

    it "can solve Part 2" $ do
      pending
      input <- readFile "inputs/day16"
      doPart2 input `shouldBe` 0
