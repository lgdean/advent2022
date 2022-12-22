module Day22Spec where
{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import Day22

spec :: Spec
spec = do
  describe "Part 1" $ do
    it "can handle given example" $ do
      -- example board map is 16x12
      input <- readFile "inputs/day22-example-map"
      password <- readFile "inputs/day22-example-password"
      doPart1 input password `shouldBe` 6032

    it "can solve Part 1" $ do
      -- puzzle board map is 150x200
      input <- readFile "inputs/day22-map"
      password <- readFile "inputs/day22-password"
      doPart1 input password `shouldBe` 1484

  describe "Part 2" $ do
    it "can handle given example" $ do
      pending
      input <- readFile "inputs/day22-example-map"
      password <- readFile "inputs/day22-example-password"
      doPart2 input password `shouldBe` 5031

    it "can solve Part 2" $ do
      pending
      input <- readFile "inputs/day22-map"
      password <- readFile "inputs/day22-password"
      doPart2 input password `shouldBe` 0
