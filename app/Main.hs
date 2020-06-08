module Main where

import Lib

main :: IO ()
main = putStrLn $ show $ (rotate 3 "ABCDEFG")
{-
data Trivial =
  Trivial'

instance Eq Trivial where
  Trivial' == Trivial' = True

--  data Identity a = Identity a
--instance Eq (Identity a) where
--(==) (Identity v) (Identity v') = v == v'

data TisAnInteger =
      TisAn Integer

instance Eq (TisAnInteger) where
     (==) (TisAn a) (TisAn a') = a == a'

data TwoIntegers = Two Integer Integer
instance Eq TwoIntegers where
    (==) (Two a b) (Two a' b') = (a == a') && (b == b')

data StringOrInt = TisAnInt Int | TisAString String
instance Eq StringOrInt where
    (==) (TisAnInt a) (TisAnInt b) = (a == b)
    (==) (TisAString a) (TisAString b) = (a == b)
    (==) _ _ = False

data Pair a = Pair a a
instance Eq a => Eq (Pair a) where
    (==) (Pair a a') (Pair b b') = (a == b) && (a' == b')


data Tuple a b = Tuple a b
instance (Eq a ,Eq b) => Eq (Tuple a b) where
    (==) (Tuple a a') (Tuple b b') = (a == b) && (a' == b')


data EitherOr a b = Hello a | Goodbye b
instance (Eq a ,Eq b) => Eq (EitherOr a b) where
    (==) (Hello a ) (Hello a') = (a == a')
    (==) (Goodbye b ) (Goodbye b') = (b == b')
    (==) _ _ = False 

bindExp :: p -> [Char]
bindExp x = let x = 10; y = 5 in
    "the integer was: " ++ show x ++ " and y was: " ++ show y

addOneIfOdd :: Integral p => p -> p
addOneIfOdd n = case odd n of
    True -> (\x -> x + 1) n
    False -> n

isItTwo :: Integer -> Bool
isItTwo _ = False
isItTwo 2 = True

functionC :: Ord p => p -> p -> p
functionC x y = case (x > y) of
    True -> x 
    False -> y

nums :: (Ord a, Num a, Num p) => a -> p
nums x =
    case compare x 0 of
        LT -> -1
        GT -> 1
        EQ -> 0


-}