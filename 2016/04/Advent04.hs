module Main where

import Data.Char (isDigit)
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Tuple (swap)

data Room = Room { getName :: Name,
                   getSID :: SecID,
                   getChk :: ChkSum }
          deriving (Eq, Show)

type Name = String
type SecID = Integer
type ChkSum = String

parseRoom :: String -> Room
parseRoom s =
    let sec1 = takeWhile (not . isDigit) s
        sec2 = takeWhile isDigit . dropWhile (not . isDigit) $ s
        sec3 = take 5 . tail . dropWhile (/='[') $ s in
    Room (init sec1)
         (read sec2)
         sec3

buildCounts :: String -> Map Char Int -> Map Char Int
buildCounts [] m = m
buildCounts (c:cs) m
  | c `M.member` m = buildCounts cs (M.adjust (+1) c m)
  | otherwise      = buildCounts cs (M.insert c 1 m)

getChkSum :: Map Char Int -> String
getChkSum = take 5 . map fst . sortOn (swap . fmap negate) . M.toList

isValidRoom :: Room -> Bool
isValidRoom (Room nm _ chk) =
    let rmMap = buildCounts (filter (/='-') nm) M.empty in
      getChkSum rmMap == chk

part1 :: String -> Integer
part1 = sidSum . filter isValidRoom . map parseRoom . lines
  where sidSum = foldr ((+) . getSID) 0

main :: IO ()
main = do
  input <- readFile "input04.txt"
  print $ part1 input

-- Test data below

testData1 :: String
testData1 = "aaaaa-bbb-z-y-x-123[abxyz]"

parsed1 :: Room
parsed1 = parseRoom testData1

testData2 :: String
testData2 = "a-b-c-d-e-f-g-h-987[abcde]"

parsed2 :: Room
parsed2 = parseRoom testData2

testData3 :: String
testData3 = "not-a-real-room-404[oarel]"

parsed3 :: Room
parsed3 = parseRoom testData3

testData4 :: String
testData4 = "totally-real-room-200[decoy]"

parsed4 :: Room
parsed4 = parseRoom testData4

initTest :: String
initTest = unlines [testData1,testData2,testData3,testData4]
