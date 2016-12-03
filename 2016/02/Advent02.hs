module Main where

testData :: String
testData = "ULL\nRRDDD\nLURDL\nUUUUD\n"

u' :: Int -> Int
u' x = if x-3 < 1 then x else x-3

d' :: Int -> Int
d' x = if x+3 > 10 then x else x+3

l' :: Int -> Int
l' x = if mod (x-1) 3 == 0 then x else x-1

r' :: Int -> Int
r' x = if mod (x+1) 3 == 1 then x else x+1

runKeySq :: Int -> String -> Int
runKeySq i [] = i
runKeySq i (x:xs) = runKeySq (nextKey x i) xs

nextKey :: Char -> Int -> Int
nextKey 'U' = u'
nextKey 'D' = d'
nextKey 'L' = l'
nextKey 'R' = r'
nextKey _   = id

test :: IO()
test = putStrLn . concatMap show . drop 1 . makeKeySq $ testData
  
makeKeySq :: String -> [Int]
makeKeySq s = let s' = lines s in
  scanl runKeySq 5 s'

-- Begin work for part 2

data KeyPad = K1 | K2 | K3 | K4 | K5 | K6 | K7
            | K8 | K9 | KA | KB | KC | KD
  deriving (Eq)

instance Show KeyPad where
    show K1 = "1"
    show K2 = "2"
    show K3 = "3"
    show K4 = "4"
    show K5 = "5"
    show K6 = "6"
    show K7 = "7"
    show K8 = "8"
    show K9 = "9"
    show KA = "A"
    show KB = "B"
    show KC = "C"
    show KD = "D"

mvUp :: KeyPad -> KeyPad
mvUp K1 = K1
mvUp K2 = K2
mvUp K3 = K1
mvUp K4 = K4
mvUp K5 = K5
mvUp K6 = K2
mvUp K7 = K3
mvUp K8 = K4
mvUp K9 = K9
mvUp KA = K6
mvUp KB = K7
mvUp KC = K8
mvUp KD = KB

mvDn :: KeyPad -> KeyPad
mvDn K1 = K3
mvDn K2 = K6
mvDn K3 = K7
mvDn K4 = K8
mvDn K5 = K5
mvDn K6 = KA
mvDn K7 = KB
mvDn K8 = KC
mvDn K9 = K9
mvDn KA = KA
mvDn KB = KD
mvDn KC = KC
mvDn KD = KD

mvLf :: KeyPad -> KeyPad
mvLf K1 = K1
mvLf K2 = K2
mvLf K3 = K2
mvLf K4 = K3
mvLf K5 = K5
mvLf K6 = K5
mvLf K7 = K6
mvLf K8 = K7
mvLf K9 = K8
mvLf KA = KA
mvLf KB = KA
mvLf KC = KB
mvLf KD = KD

mvRt :: KeyPad -> KeyPad
mvRt K1 = K1
mvRt K2 = K3
mvRt K3 = K4
mvRt K4 = K4
mvRt K5 = K6
mvRt K6 = K7
mvRt K7 = K8
mvRt K8 = K9
mvRt K9 = K9
mvRt KA = KB
mvRt KB = KC
mvRt KC = KC
mvRt KD = KD

trInst :: Char -> KeyPad -> KeyPad
trInst 'U' = mvUp
trInst 'D' = mvDn
trInst 'L' = mvLf
trInst 'R' = mvRt
trInst _   = id

execInst :: KeyPad -> String -> KeyPad
execInst = foldl (flip trInst)

makeKeySq' :: String -> [KeyPad]
makeKeySq' = scanl execInst K5 . lines

-- Main

main :: IO ()
main = do
  input <- readFile "input02.txt"
  putStrLn . concatMap show . drop 1 . makeKeySq $ input -- Part 1
  putStrLn . concatMap show . drop 1 . makeKeySq' $ input -- Part 2
  
