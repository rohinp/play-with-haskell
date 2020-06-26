module Chapter9 where

import Data.Char

capitalize :: [Char] -> [Char]
capitalize [] = []
capitalize (x:xs) = (toUpper x) : xs

capitalizeAll xs = [toUpper x | x <- xs]

capitalizedHead = toUpper . head

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f xs = concat [f x | x <- xs]

squishMap' f xs = concat $ map f xs

squishMap'' :: Monad m => (a -> m b) -> m a -> m b
squishMap'' f xs = xs >>= f
