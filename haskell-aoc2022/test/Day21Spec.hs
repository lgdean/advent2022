module Day21Spec where
{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import Day21

spec :: Spec
spec = do
  describe "Part 1" $ do
    it "can handle given example" $ do
      input <- readFile "inputs/day21-example"
      doPart1 input `shouldBe` 152

    it "can solve Part 1" $ do
      input <- readFile "inputs/day21"
      doPart1 input `shouldBe` 21208142603224

  describe "Part 2" $ do
    it "can handle given example" $ do
      pending
      input <- readFile "inputs/day21-example"
      doPart2 input `shouldBe` undefined

    it "can solve Part 2" $ do
      pending
      input <- readFile "inputs/day21"
      doPart2 input `shouldBe` 0
