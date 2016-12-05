module Main where

import Text.Trifecta

type Length = Integer
type Width  = Integer
type Height = Integer
type Package = (Length,Width,Height)

parseDims :: Parser Package
parseDims = do
    l <- integer
    char 'x'
    w <- integer
    char 'x'
    h <- integer
    return (l,w,h)

minSideArea :: Package -> Integer
minSideArea (l,w,h) = minimum [l*w,w*h,h*l]

minPerim :: Package -> Integer
minPerim (l,w,h) = minimum [2*(l+w),2*(w+h),2*(h+l)]

surfArea :: Package -> Integer
surfArea (l,w,h) = 2*l*w + 2*w*h + 2*h*l

volume :: Package -> Integer
volume (l,w,h) = l * w * h

paperArea :: Package -> Integer
paperArea p = surfArea p + minSideArea p

ribbonLength :: Package -> Integer
ribbonLength p = minPerim p + volume p

totalPaper :: [Package] -> Integer
totalPaper = sum . map paperArea

totalRibbon :: [Package] -> Integer
totalRibbon = sum . map ribbonLength

main :: IO ()
main = do
  input <- readFile "input02.txt"
  let pkgs = parseString (some parseDims) mempty input
  putStrLn $ "1: " ++ (show $ fmap totalPaper pkgs)
  putStrLn $ "2: " ++ (show $ fmap totalRibbon pkgs)
