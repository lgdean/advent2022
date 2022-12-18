module Day18Spec where
{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import Day18

spec :: Spec
spec = do
  describe "Part 1" $ do
    it "can handle given tiny example" $ do
      let input = "1,1,1\n2,1,1"
      doPart1 input `shouldBe` 10

    it "can handle given example" $ do
      input <- readFile "inputs/day18-example"
      doPart1 input `shouldBe` 64

    it "can solve Part 1" $ do
      input <- readFile "inputs/day18"
      doPart1 input `shouldBe` 3346

  describe "Part 2" $ do
    it "can handle given example" $ do
      pending
      input <- readFile "inputs/day18-example"
      doPart2 input `shouldBe` undefined

    it "can solve Part 2" $ do
      pending
      input <- readFile "inputs/day18"
      doPart2 input `shouldBe` 0
