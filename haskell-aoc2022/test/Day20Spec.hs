module Day20Spec where
{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import Day20

spec :: Spec
spec = do
  describe "Part 1" $ do
    it "can mix file one time as in given example" $ do
      input <- readFile "inputs/day20-example"
      readAndMixFile input `shouldBe` [1, 2, -3, 4, 0, 3, -2]

    it "can handle given example" $ do
      input <- readFile "inputs/day20-example"
      doPart1 input `shouldBe` 3

    it "can solve Part 1" $ do
      input <- readFile "inputs/day20"
      doPart1 input `shouldBe` 5962

  describe "Part 2" $ do
    it "can mix file one time as in given example, with correct first item" $ do
      input <- readFile "inputs/day20-example"
      let numbers = map ((* 811589153) . read) $ lines input :: [Int]
      mixFile 1 numbers `shouldBe` [0, -2434767459, 3246356612, -1623178306, 2434767459, 1623178306, 811589153]

    it "can handle given example" $ do
      input <- readFile "inputs/day20-example"
      doPart2 input `shouldBe` 1623178306

    it "can solve Part 2" $ do
      input <- readFile "inputs/day20"
      doPart2 input `shouldBe` 9862431387256
