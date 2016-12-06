module Main where

import Control.Monad (when)
import Data.Map (Map)
import qualified Data.Map as M

type Houses = Map (Int,Int) Int
type XY = (Int,Int)

runDir :: Char -> XY -> XY
runDir  '^' (a,b) = (a,b+1)
runDir  'v' (a,b) = (a,b-1)
runDir  '>' (a,b) = (a+1,b)
runDir  '<' (a,b) = (a-1,b)

addOrInc :: XY -> Houses -> Houses
addOrInc p = M.insertWith (+) p 1

buildVisited :: String -> XY -> Houses -> Houses
buildVisited [] p m     = addOrInc p m
buildVisited (s:xs) p m = let p' = runDir s p in
  buildVisited xs p' (addOrInc p m)

buildVisited' :: String -> XY -> XY -> Houses -> Houses
buildVisited' [] pSanta pRobot m =
  addOrInc pSanta (addOrInc pRobot m)
buildVisited' (s:r:xs) pSanta pRobot m =
  let ps = runDir s pSanta
      pr = runDir r pRobot in
        buildVisited' xs ps pr (addOrInc pSanta (addOrInc pRobot m))

debug = False

main :: IO()
main = do
  input <- readFile "input03.txt"
  when debug (print . length $ input)
  putStrLn $ "1: " ++ show (length . buildVisited input (0,0) $ M.empty)
  putStrLn $ "1: " ++ show (length . buildVisited' input (0,0) (0,0) $ M.empty)
