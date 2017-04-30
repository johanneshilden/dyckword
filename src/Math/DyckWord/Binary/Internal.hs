module Math.DyckWord.Binary.Internal where

import Math.Combinatorics.Exact.Binomial

catalanTriangle :: Integer -> Integer -> Integer
catalanTriangle _ 0 = 1
catalanTriangle n k = (choose (n + k) (k - 1) * (n - k + 1)) `div` k 

catalanSum :: Int -> Integer
catalanSum = (scanl (+) 0 (catalan <$> [0..]) !!)

catalan :: (Integral a, Integral b) => a -> b
catalan 0 = 1
catalan 1 = 1
catalan 2 = 2
catalan n = let m = fromIntegral n in (2*m) `choose` m `div` (m + 1) 
