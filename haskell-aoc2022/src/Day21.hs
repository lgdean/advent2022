module Day21
    (
      doPart1,
      doPart2
    ) where

import Data.List.Split (splitOn)
import Data.Map (Map, (!))
import qualified Data.Map.Strict as Map

import Debug.Trace (trace)

data Op = Plus | Minus | Times | Divide deriving (Eq, Show)
data Expr = Simple Int | Figure Op String String deriving (Show)

type MonkeyJobs = Map String Expr

monkeyYells :: MonkeyJobs -> String -> Int
monkeyYells jobs var =
  calculate jobs $ jobs ! var

-- can be this simple for now; no concept of waiting
calculate :: MonkeyJobs -> Expr -> Int
calculate _ (Simple x) = x
calculate jobs (Figure op x y) =
  let xVal = monkeyYells jobs x
      yVal = monkeyYells jobs y
  in operate op xVal yVal

operate :: Op -> Int -> Int -> Int
operate Plus = (+)
operate Minus = (-)
operate Times = (*)
operate Divide = div -- presumably this kind of dividing

doPart1 :: String -> Int
doPart1 input =
  let allLines = lines input
      monkeyBusiness = map parseLine allLines
      monkeyJobs = Map.fromList monkeyBusiness
      answer = monkeyYells monkeyJobs "root"
  in answer

doPart2 :: String -> Int
doPart2 input =
  let _allLines = lines input
  in 0


parseLine :: String -> (String, Expr)
parseLine input =
  let parts = splitOn ":" input
      monkeyName = head parts
      expr = parseExpr $ head $ tail parts
  in (monkeyName, expr)

parseExpr :: String -> Expr
parseExpr input =
  case words input of
    [x] -> Simple (read x)
    [a, op, b] -> Figure (parseOp op) a b
    _ -> error ("cannot parse expression: " ++ input)

parseOp :: String -> Op
parseOp "+" = Plus
parseOp "-" = Minus
parseOp "*" = Times
parseOp "/" = Divide
parseOp anythingElse = error ("cannot parse op "++anythingElse)
