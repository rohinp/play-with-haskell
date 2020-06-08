module Lib where 

import Safe
import Data.Maybe
import Data.Bifoldable

rotate :: Int -> String -> String
rotate _ [] = []
rotate n chars@(x:xs)
    | n == 0 = chars
    | n < 0 = error "Less than usual"
    | n > (length chars) = error "Too big"
    | otherwise = rotate (n-1) (xs ++ [x]) 


prop_rotate :: Int -> String -> Bool
prop_rotate k str = rotate (l - m) (rotate m str) == str
                        where l = length str
                              m = if l == 0 then 0 else k `mod` l


makeKey :: Int -> [(Char, Char)]
makeKey n = zip alphabet (rotate n alphabet)
    where alphabet = ['A'..'Z']

-- 

lookupRec :: Char -> [(Char, Char)] -> Char
lookupRec c [] = c
lookupRec c ((k,v):kvs) 
    | k == c = v
    | otherwise = lookupRec c kvs

lookUp :: Char -> [(Char, Char)] -> Char
lookUp c xs = fromMaybe c $ headMay [ b | (a,b) <- xs, a == c] 

lookUp1 :: Char -> [(Char, Char)] -> Char
lookUp1 c xs
    | null v = c
    | otherwise = head v
    where 
        v = [ b | (a,b) <- xs, a == c]

prop_lookUp :: Char -> [(Char, Char)] -> Bool
prop_lookUp c xs = lookUp c xs == lookupRec c xs