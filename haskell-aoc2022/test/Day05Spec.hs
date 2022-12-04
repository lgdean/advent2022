module Day05Spec where
{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import Day05

spec :: Spec
spec = do
  describe "Part 1" $ do
    it "can handle given example" $ do
      pending
      input <- readFile "inputs/day05-example"
      doPart1 input `shouldBe` undefined

    it "can solve Part 1" $ do
      pending
      input <- readFile "inputs/day05"
      doPart1 input `shouldBe` 0

  describe "Part 2" $ do
    it "can handle given example" $ do
      pending
      input <- readFile "inputs/day05-example"
      doPart2 input `shouldBe` undefined

    it "can solve Part 2" $ do
      pending
      input <- readFile "inputs/day05"
      doPart2 input `shouldBe` 0
