module Main where
import Control.Monad
import Text.Printf

main :: IO()
main = forever $ resolve . map read . words <$> getLine >>= out where
  resolve :: (RealFloat n) => [n] -> (n, n)
  resolve (a:b:p:c:d:q:_) = (x, y) where
    x = (p*d - q*b) / (a*d - b*c)
    y = (p*c - q*a) / (b*c - a*d)
  resolve _ = error "unknown"
  out :: (Float, Float) -> IO()
  out (x, y) = printf "%.3f %.3f" x y
