module Main where

upOrDown :: Char -> Integer
upOrDown '(' = 1
upOrDown ')' = -1
upOrDown  _  = 0

getFloor :: String -> Integer
getFloor = foldr ((+) . upOrDown) 0

getPos :: Integer -> String -> Maybe Integer
getPos fl xs = 
    let ys = zip xs [1..]
        pos = scanl (\x (a,b) -> (fst x + upOrDown a, b)) (0, 0) ys in
          lookup fl pos

main :: IO ()
main = do
    input <- readFile "input01.txt"
    putStrLn $ "1: " ++ (show $ getFloor input)
    putStrLn $ "2: " ++ (show . getPos (-1) . concat . lines $ input)
