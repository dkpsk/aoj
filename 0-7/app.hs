module Main where

main :: IO()
main = do
  n <- readLn :: IO Int
  print.ceiling $ (iterate explode 100000) !! n

explode :: Float -> Float
explode v | v == 0 = 0
          | otherwise = realToFrac . (*1000) . ceiling $ v * 1.05 / 1000
