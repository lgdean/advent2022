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
      input <- readFile "inputs/day18-example"
      doPart2 input `shouldBe` 58

    it "can handle a solid cube" $ do
      input <- readFile "inputs/day18-cube-solid"
      doPart2 input `shouldBe` 150

    it "can handle a hollow cube" $ do
      input <- readFile "inputs/day18-cube"
      doPart2 input `shouldBe` 150

    it "can handle a hollow almost-cube (one piece missing from face)" $ do
      input <- readFile "inputs/day18-cube-almost"
      doPart2 input `shouldBe` 206

    it "can solve Part 2" $ do
      input <- readFile "inputs/day18"
      -- 1974 is too low, as is 1976
      doPart2 input `shouldBe` 1980
