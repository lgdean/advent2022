module Day05Spec where
{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import Day05

spec :: Spec
spec = do
  describe "Part 1" $ do
    it "can handle given example" $ do
      input <- readFile "inputs/day05-example"
      let startState = [ (1, ['N','Z']), (2, ['D','C', 'M']), (3, ['P'])]
      doPart1 startState input `shouldBe` "CMZ"

    it "can solve Part 1" $ do
      input <- readFile "inputs/day05"
      let startState = [
                         (1, ['D','H','R','Z','S','P','W','Q'])
                       , (2, ['F','H','Q','W','R','B','V'])
                       , (3, ['H','S','V','C'])
                       , (4, ['G','F','H'])
                       , (5, ['Z','B','J','G','P'])
                       , (6, ['L','F','W','H','J','T','Q'])
                       , (7, ['N','J','V','L','D','W','T','Z'])
                       , (8, ['F','H','G','J','C','Z','T','D'])
                       , (9, ['H','B','M','V','P','W'])
                      ]
      doPart1 startState input `shouldBe` "ZWHVFWQWW"

  describe "Part 2" $ do
    it "can handle given example" $ do
      pending
      input <- readFile "inputs/day05-example"
      doPart2 input `shouldBe` undefined

    it "can solve Part 2" $ do
      pending
      input <- readFile "inputs/day05"
      doPart2 input `shouldBe` 0
