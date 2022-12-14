module Day14Spec where
{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import Day14

spec :: Spec
spec = do
  describe "Part 1" $ do
    it "can handle given example" $ do
      input <- readFile "inputs/day14-example"
      doPart1 input `shouldBe` 24

    it "can solve Part 1" $ do
      input <- readFile "inputs/day14"
      doPart1 input `shouldBe` 1061

  describe "Part 2" $ do
    it "can handle given example" $ do
      pending
      input <- readFile "inputs/day14-example"
      doPart2 input `shouldBe` undefined

    it "can solve Part 2" $ do
      pending
      input <- readFile "inputs/day14"
      doPart2 input `shouldBe` 0
