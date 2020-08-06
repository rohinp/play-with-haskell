
module Addition where

import Test.Hspec

sayHello :: IO ()
sayHello = putStrLn "hello!"

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
    where go n d count
            | n < d = (count, n)
            | otherwise = go (n - d) d (count + 1)


summation :: (Eq a, Num a) => a -> a
summation 0 = 0
summation n = summation (n -1) + n

multiplies :: (Eq a, Ord a, Num a) => a -> a -> a
multiplies x y
    | x > 0     = y + multiplies (x - 1) y
    | x < 0     = (negate y) + multiplies (x + 1) y
    | otherwise = 0
