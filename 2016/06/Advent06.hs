{-# LANGUAGE QuasiQuotes #-}
module Main where

import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Tuple (swap)
import Text.RawString.QQ

getCounts :: String -> Map Char Integer -> Map Char Integer
getCounts "" m = m
getCounts (s:xs) m = getCounts xs (M.insertWith (+) s 1 m)

mostCommon :: String -> Char
mostCommon s = let counts = getCounts s M.empty in
  fst . head . sortOn (swap . fmap negate) . M.toList $ counts

leastCommon :: String -> Char
leastCommon s = let counts = getCounts s M.empty in
  fst . head . sortOn (swap) . M.toList $ counts

translate :: [String] -> String
translate = map mostCommon . transpose

translate' :: [String] -> String
translate' = map leastCommon . transpose

main :: IO ()
main = do
    input <- readFile "input06.txt"
    putStrLn $ "1: " ++ (translate . lines $ input)
    putStrLn $ "2: " ++ (translate' . lines $ input)

testData = [r|eedadn
drvtee
eandsr
raavrd
atevrs
tsrnev
sdttsa
rasrtv
nssdts
ntnada
svetve
tesnvt
vntsnd
vrdear
dvrsen
enarar
|]
