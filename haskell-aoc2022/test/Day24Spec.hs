module Day24Spec where
{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import Day24

spec :: Spec
spec = do
  describe "Part 1" $ do
    it "can handle given example" $ do
      input <- readFile "inputs/day24-example"
      doPart1 input `shouldBe` 18

    it "can solve Part 1" $ do
      input <- readFile "inputs/day24"
      doPart1 input `shouldBe` 247

  describe "Part 2" $ do
    it "can handle given example" $ do
      pending
      input <- readFile "inputs/day24-example"
      doPart2 input `shouldBe` undefined

    it "can solve Part 2" $ do
      pending
      input <- readFile "inputs/day24"
      doPart2 input `shouldBe` 0
