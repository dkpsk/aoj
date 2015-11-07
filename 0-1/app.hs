module Main where

import System.Environment(getArgs)
import Data.List(sort)
import Control.Monad(join)

main :: IO()
main = pLn $ (take 3 . reverse . sort . fmap read <$> getArgs :: IO[Int])

pLn :: (Show a) => IO[a] -> IO()
pLn x = join $ fmap f x where
  f :: (Show a) => [a] -> IO()
  f xs = sequence_ $ fmap (putStrLn . show) xs
