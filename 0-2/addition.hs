module Main where

import System.Environment

main :: IO()
main = add <$> getArgs >>= p2 . p1
  where
  add :: [String] -> [Int]
  add [] = []
  add [_] = []
  add (x:y:r) = read x + read y : add r 

  keta :: Int -> Int
  keta = length . show
  
  p1 :: [Int] -> [IO()]
  p1 is = fmap (putStrLn . show . length . show) is

  p2 :: [IO()] -> IO()
  p2 = sequence_
