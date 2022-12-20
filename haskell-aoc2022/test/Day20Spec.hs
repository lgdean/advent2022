module Day20Spec where
{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import Day20

spec :: Spec
spec = do
  describe "Part 1" $ do
    it "can mix file one time as in given example" $ do
      input <- readFile "inputs/day20-example"
      readAndMixFile input `shouldBe` [1, 2, -3, 4, 0, 3, -2]

    it "can handle given example" $ do
      input <- readFile "inputs/day20-example"
      doPart1 input `shouldBe` 3

    it "can solve Part 1" $ do
      input <- readFile "inputs/day20"
      doPart1 input `shouldBe` 5962

  describe "Part 2" $ do
    it "can handle given example" $ do
      pending
      input <- readFile "inputs/day20-example"
      doPart2 input `shouldBe` undefined

    it "can solve Part 2" $ do
      pending
      input <- readFile "inputs/day20"
      doPart2 input `shouldBe` 0
