module Day13
    (
      doPart1,
      doPart2
    ) where

import Data.Char (isDigit)
import Data.List (elemIndex, sort)
import Data.Maybe (catMaybes)

import Lib (parseChunks)

data Value = Single Int | MyList [Value] deriving Show

instance Eq Value where
  (Single a) == (Single b) = a == b
  (MyList a) == (MyList b) = a == b
  _ == _ = False                    -- will this cause problems? OK for now.

instance Ord Value where
  compare (Single a) (Single b) = compare a b
  compare (MyList []) (MyList []) = EQ -- or can I just use list eq?
  compare (MyList _) (MyList []) = GT
  compare (MyList []) (MyList _) = LT
  compare (MyList a) (MyList b) = compare a b -- can this cover previous cases too?
  compare (MyList a) (Single b) = compare (MyList a) (MyList [Single b])
  compare (Single a) (MyList b) = compare (MyList [Single a]) (MyList b)

doPart1 :: String -> Int
doPart1 input =
  let parsedPairs = parseChunks parsePackets input
      areTheyCorrect = map (uncurry (<=)) parsedPairs
      correctPairs = filter snd $ zip [1..] areTheyCorrect
      answer = sum $ map fst correctPairs
  in answer

doPart2 :: String -> Int
doPart2 input =
  let parsedPairs = parseChunks parsePackets input
      asLists = map (\(a,b) -> [a,b]) parsedPairs
      dividerPackets = map (fst . parseMyList) ["[[2]]", "[[6]]"]
      allPackets = concat (dividerPackets : asLists)
      sorted = sort allPackets
      dividerIndices = map (`elemIndex` sorted) dividerPackets
      answer = product $ map succ $ catMaybes dividerIndices
  in answer

parsePackets :: [String] -> (Value, Value)
parsePackets [one, other] =
  case (parseMyList one, parseMyList other) of
    ((valOne, ""), (valOther, "")) -> (valOne, valOther)
    ((_, rest), _) -> error ("unexpected rest of first line in pair: "++rest)
    (_, (_, rest)) -> error ("unexpected rest of second line in pair: "++rest)
    _              -> error "IDEK"
parsePackets _ = error "parsing wrong number of packets"

parseMyList :: String -> (Value, String)
parseMyList "" = error "ran out of string when trying to make a MyList"
parseMyList ('[':']':rest) = (MyList [], rest)
parseMyList ('[':rest) =
  let (innerVal, remainder) = parseListItem rest
  in case remainder of
     (']':evenMore) -> (MyList [innerVal], evenMore)
     (',':evenMore) -> let (moreItems, arghNaming) = parseRestOfList (',':evenMore)
                       in (MyList (innerVal:moreItems), arghNaming)
     _ -> error ("parseMyList cannot handle remainder: "++remainder)

parseListItem :: String -> (Value, String)
parseListItem "" = error "this seems unlikely"
parseListItem ('[':rest) = parseMyList ('[':rest)
parseListItem (']':rest) = (MyList [], ']':rest)
parseListItem input =
  let (digits, nxt) = span isDigit input
      resultInt = read digits :: Int
  in (Single resultInt, nxt)

parseRestOfList :: String -> ([Value], String)
parseRestOfList "" = error "bad time to call parseRestOfList"
parseRestOfList (']':rest) = ([], rest)
parseRestOfList (',':input) =
  let (item, nxt) = parseListItem input
      (moreItems, rest) = parseRestOfList nxt
  in (item:moreItems, rest)
parseRestOfList other = error ("parseRestOfList cannot parse: "++other)
