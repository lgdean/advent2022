module Day07Spec where
{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import Day07

spec :: Spec
spec = do
  describe "Part 1" $ do
    it "can handle given example" $ do
      input <- readFile "inputs/day07-example"
      doPart1 input `shouldBe` 95437

    it "can solve Part 1" $ do
      input <- readFile "inputs/day07"
      doPart1 input `shouldBe` 1141028

  describe "Part 2" $ do
    it "can handle first given example" $ do
      pending
      doPart2 "mjqjpqmgbljsphdztnvjfqwrcgsmlb" `shouldBe` undefined

    it "can solve Part 2" $ do
      pending
      input <- readFile "inputs/day07"
      doPart2 input `shouldBe` 0
