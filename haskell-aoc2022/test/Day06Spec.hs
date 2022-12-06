module Day06Spec where
{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import Day06

spec :: Spec
spec = do
  describe "Part 1" $ do
    it "can handle first given example" $ do
      doPart1 "mjqjpqmgbljsphdztnvjfqwrcgsmlb" `shouldBe` 7

    it "can handle next given example" $ do
      doPart1 "bvwbjplbgvbhsrlpgdmjqwftvncz" `shouldBe` 5

    it "can solve Part 1" $ do
      input <- readFile "inputs/day06"
      doPart1 input `shouldBe` 1578

  describe "Part 2" $ do
    it "can handle first given example" $ do
      doPart2 "mjqjpqmgbljsphdztnvjfqwrcgsmlb" `shouldBe` 19

    it "can handle next given example" $ do
      doPart2 "bvwbjplbgvbhsrlpgdmjqwftvncz" `shouldBe` 23

    it "can solve Part 2" $ do
      input <- readFile "inputs/day06"
      doPart2 input `shouldBe` 2178
