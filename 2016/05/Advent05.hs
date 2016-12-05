{-# LANGUAGE OverloadedStrings #-}

module Main where

import Crypto.Hash
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Maybe (mapMaybe)
import Data.String (IsString, fromString)

md5 :: ByteString -> Digest MD5
md5 = hash

test :: ByteString
test = "abc"

myInput :: ByteString
myInput = "ojvtpuvg"

myInputLst :: ByteString -> [ByteString]
myInputLst xs = map ((xs `B.append`) . C.pack . show) [1..]

md5list :: [ByteString] -> [String]
md5list = map (take 7 . show . md5)

password :: ByteString -> String
password = take 8 . map (!! 5) . filter (\x -> take 5 x == "00000") . md5list . myInputLst

password' :: ByteString -> [(Char,Char)]
password' = let go = filter (\x -> take 5 x == "00000") . md5list . myInputLst in
  filter go' . map ((\[a,b] -> (a,b)) . drop 5) . go where
    go' xs = fst xs `elem` ("01234567" :: String)

get2ndPass :: [(Char,Char)] -> String
get2ndPass xs = mapMaybe (`lookup` xs) "01234567"

main :: IO ()
main = putStrLn (password myInput) >> putStrLn (get2ndPass . password' $ myInput)
