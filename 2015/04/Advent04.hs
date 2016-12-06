{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Arrow (first)
import Crypto.Hash
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

md5 :: ByteString -> Digest MD5
md5 = hash

myInput :: ByteString
myInput = "ckczppom"

inputList :: ByteString -> [(ByteString,Integer)]
inputList xs = map (go xs) [1..]
  where go ys i = (xs `B.append` C.pack (show i), i)

md5list :: [(ByteString,Integer)] -> [(String,Integer)]
md5list = map (first (show . md5))

numGet :: ByteString -> (String,Integer)
numGet = head . filter (\x -> take 5 (fst x) == "00000") . md5list . inputList

numGet6 :: ByteString -> (String,Integer)
numGet6 = head . filter (\x -> take 6 (fst x) == "000000") . md5list . inputList

main :: IO ()
main = do
    putStrLn $ "1: " ++ show (numGet myInput)
    putStrLn $ "2: " ++ show (numGet6 myInput)
