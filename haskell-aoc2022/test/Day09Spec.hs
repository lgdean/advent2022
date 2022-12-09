module Day09Spec where
{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import Day09

spec :: Spec
spec = do
  describe "Part 1" $ do
    it "can handle given example" $ do
      input <- readFile "inputs/day09-example"
      doPart1 input `shouldBe` 13

    it "can solve Part 1" $ do
      input <- readFile "inputs/day09"
      doPart1 input `shouldBe` 6087

  describe "Part 2" $ do
    it "can handle given example" $ do
      pending
      input <- readFile "inputs/day09-example"
      doPart2 input `shouldBe` undefined

    it "can solve Part 2" $ do
      pending
      input <- readFile "inputs/day09"
      doPart2 input `shouldBe` 0
