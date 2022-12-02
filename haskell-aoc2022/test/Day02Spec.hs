module Day02Spec where
{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import Day02

spec :: Spec
spec = do
  describe "Part 1" $ do
    it "can handle given example" $ do
      input <- readFile "inputs/day02-example"
      doPart1 input `shouldBe` 15

    it "can solve Part 1" $ do
      input <- readFile "inputs/day02"
      doPart1 input `shouldBe` 9759
