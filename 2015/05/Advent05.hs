module Main where

import Data.List

has3Vowels :: String -> Bool
has3Vowels s =
    length (filter isVowel s) >= 3
    where isVowel c = c `elem` "aeiou"

hasDouble :: String -> Bool
hasDouble [] = False
hasDouble [x] = False
hasDouble (a:b:xs) = a == b || hasDouble (b:xs)

noBadSubstr :: String -> [String] -> Bool
noBadSubstr s =  not . any (`isInfixOf` s)

badSubstrings :: [String]
badSubstrings = ["ab","cd","pq","xy"]

isNice :: String -> Bool
isNice s = has3Vowels s && hasDouble s && noBadSubstr s badSubstrings

hasPair :: String -> Bool
hasPair []  = False
hasPair [x] = False
hasPair (a:b:xs) = [a,b] `isInfixOf` xs || hasPair (b:xs)

hasABApattern :: String -> Bool
hasABApattern []    = False
hasABApattern [x]   = False
hasABApattern [x,y] = False
hasABApattern (a:b:c:xs) = a == c || hasABApattern (b:c:xs)

isNice' :: String -> Bool
isNice' s = hasPair s && hasABApattern s

main :: IO ()
main = do
    input <- readFile "input05.txt"
    let numNice = length . filter isNice . lines $ input
    let numNice' = length . filter isNice' . lines $ input
    putStrLn $ "1: " ++ show numNice
    putStrLn $ "2: " ++ show numNice'
