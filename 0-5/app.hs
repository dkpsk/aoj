module Main where
import Prelude hiding (gcd , lcm)

main :: IO()
main =  (fmap (go . map read) $ fmap words getLine) >>= \(a,b) -> (putStr $ show a) >> putStr " " >> (putStrLn $ show b) where
  go :: [Int] -> (Int, Int)
  go [m, n] = let (a, b) = if m > n then (m, n) else (n, m) in (gcd a b, lcm a b)

-- suppose: m > n
gcd :: (Integral n) => n -> n -> n
gcd m n = let r = m `rem` n in if r == 0 then n else gcd n r

lcm :: (Integral n) => n -> n -> n
lcm m n = (m * n) `quot` gcd m n
