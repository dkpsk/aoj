module Main where

import Control.Monad

main :: IO ()
main = forever $ read <$> getLine >>=  solve

solve :: Int -> IO()
solve 0 = return ()
solve x = print $ (length . findPairs) x

type Prime = Int

primes :: [Prime]
primes = 2 : filter isPrime [3,5..]

-- ���Τ��ǿ��Τ�����Ǥ���ʤ��ʤ��ǿ�
isPrime ::  Int -> Bool
isPrime n = all f ps' where
  ps' = takeWhile (\p -> p*p <= n) primes
  f p = n `mod` p /= 0

findPairs :: Int -> [(Prime, Prime)]
findPairs x = solve' primes x [] where
  solve' :: [Prime] -> Int -> [(Prime, Prime)] -> [(Prime, Prime)]
  solve' [] _ ans = ans
  solve' (p:ps) n ans = let s = n - p in
    if p > s then ans
    else if isPrime s then solve' ps n ((p, s):ans)
    else solve' ps n ans
