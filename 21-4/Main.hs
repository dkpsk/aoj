module Main where
import Data.List
import Control.Monad

main :: IO ()
main = do
  t <- read <$> getLine :: IO Int
  replicateM_ t $ execute >>= print
    where
      execute ::  IO Int
      execute = do
        [n, k] <- fmap (fmap read) $ fmap words getLine :: IO [Int]
        hs <- fmap (fmap read) $ fmap words getLine :: IO [Int]
        return $ calculate n k hs

calculate :: Int -> Int -> [Int] -> Int
calculate n k pos
  | n <= k = 0
  | otherwise = let ds = distance pos
                    l  = sum ds
                    e = sum $ take (k-1) $ sortBy (flip compare) ds
                in
                    l - e

-- 家と家の間の距離のリスト
distance :: [Int] -> [Int]
distance hs = fmap (uncurry $ flip (-)) $ f hs where
  f :: [a] -> [(a, a)]
  f [] = []
  f [_] = []
  f x@(a:b:_) = (a, b) : f (tail x)
