module Day12Spec where
{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import Day12

spec :: Spec
spec = do
  describe "Part 1" $ do
    it "can handle given example" $ do
      input <- readFile "inputs/day12-example"
      doPart1 input `shouldBe` 31

    it "can solve Part 1" $ do
      input <- readFile "inputs/day12"
      doPart1 input `shouldBe` 339

  describe "Part 2" $ do
    it "can handle given example" $ do
      pending
      input <- readFile "inputs/day12-example"
      doPart2 input `shouldBe` undefined

    it "can solve Part 2" $ do
      pending
      input <- readFile "inputs/day12"
      doPart2 input `shouldBe` 0
