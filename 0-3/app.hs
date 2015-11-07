module Main where
import System.Environment(getArgs)
import Data.List(sort)

main :: IO ()
main = f . drop 1 . lines . head <$> getArgs >>= p where
  f :: [String] -> [Bool]
  f(x:xs) = isT (sort $ fmap read $ words x) : f xs
  f [] = []
  p :: [Bool] -> IO()
  p bs = mapM_ (putStrLn . g ) bs where g b = if b then "YES" else "NO"
  isT :: [Int] -> Bool
  isT (a:b:c:_) = c^2 == a^2 + b^2
  isT [] = False
  isT _ = error "unknown pattern"
