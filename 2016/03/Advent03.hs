module Main where

import Data.List

isValidTri :: Int -> Int -> Int -> Bool
isValidTri a b c
  | a <= 0 || b <= 0 || c <= 0 = False
  | c >= a && c >= b = (a+b) > c
  | b >= a && b >= c = (a+c) > b
  | a >= b && a >= c = (b+c) > a

isValidTri' :: (Int,Int,Int) -> Bool
isValidTri' (a,b,c) = isValidTri a b c

rInt :: String -> Int
rInt = read

part1 :: String -> Int
part1 s = 
  let dInput = map ((\[x,y,z] -> (x,y,z)) . map rInt . words) . lines $ s in
    length . filter id . map isValidTri' $ dInput

swap3s :: [[a]] -> [[a]]
swap3s [] = []
swap3s [a] = error "Input not consumed. One element left."
swap3s [a,b] = error "Input not consumed. Two elements left."
swap3s (x:y:z:zs) = transpose [x,y,z] ++ swap3s zs

part2 :: String -> Int
part2 s = 
  let dInput = 
        map (\[x,y,z] -> (x,y,z)) . swap3s . map (map rInt . words) . lines $ s in
    length . filter id . map isValidTri' $ dInput

main :: IO ()
main = do
    input <- readFile "input03.txt"
    print . part1 $ input
    print . part2 $ input
