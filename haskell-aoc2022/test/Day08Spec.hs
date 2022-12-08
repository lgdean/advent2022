module Day08Spec where
{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import Day08

spec :: Spec
spec = do
  describe "Part 1" $ do
    it "can handle given example" $ do
      input <- readFile "inputs/day08-example"
      doPart1 input `shouldBe` 21

    it "can solve Part 1" $ do
      input <- readFile "inputs/day08"
      doPart1 input `shouldBe` 1832

  describe "Part 2" $ do
    it "can handle given example" $ do
      pending
      input <- readFile "inputs/day08-example"
      doPart2 input `shouldBe` undefined

    it "can solve Part 2" $ do
      pending
      input <- readFile "inputs/day08"
      doPart2 input `shouldBe` 0
