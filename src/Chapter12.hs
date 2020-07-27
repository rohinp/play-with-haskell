module Chapter12 where

 
 {-
 id :: a -> a
 :k a is *

 r :: a -> f a
 :k f is * -> *


 -}

 -- String processing

 notThe :: String -> Maybe String
 notThe xs = if(xs == "the") then Nothing else Just xs

 nothingToA :: Maybe String -> String
 nothingToA Nothing = "a"
 nothingToA (Just xs) = xs

 replaceThe :: String -> String
 replaceThe = unwords . map (nothingToA . notThe) . words

 replaceThe' :: String -> String
 replaceThe' "" = ""
 replaceThe' ('t':'h':'e':' ':xs) = "a " ++ replaceThe' xs
 replaceThe' xs = first xs ++ " " ++ (replaceThe' $ remaining xs)
    where
        first = head . words 
        remaining = unwords . tail . words

 isVowel :: Char -> Bool
 isVowel c = any (== c) "aeiou"

 countTheBeforeVowel :: String -> Integer
 countTheBeforeVowel "" = 0
 countTheBeforeVowel ('t':'h':'e':' ':x:xs)
    | isVowel x = 1 + countTheBeforeVowel (x:xs)
    | otherwise = countTheBeforeVowel (x:xs)
 countTheBeforeVowel xs = countTheBeforeVowel $ unwords $ tail $ words xs

 -- Validate the word
 newtype Word' = Word' String deriving (Eq, Show)
 
 vowels :: [Char]
 vowels = "aeiou"
 
 constantVowelCount:: String -> (Int, Int)
 constantVowelCount "" = (0,0)
 constantVowelCount xs = loop xs 0 0
    where 
        loop:: String -> Int -> Int -> (Int, Int)
        loop "" c v= (c,v)
        loop (x:xs) c v
            | any (==x) vowels = loop xs c (v+1)
            | otherwise = loop xs (c+1) v


 mkWord :: String -> Maybe Word'
 mkWord xs 
    | vowelCount > constCount = Nothing
    | otherwise = Just $ Word' xs
    where
        (constCount, vowelCount) = constantVowelCount xs

 data Nat = Zero | Succ Nat deriving (Eq, Show)
 
 natToInteger :: Nat -> Integer
 natToInteger Zero = 0
 natToInteger (Succ n) = 1 + natToInteger n

 integerToNat :: Integer -> Maybe Nat
 integerToNat x
    | x < 0 = Nothing
    | x == 0 = Just Zero
    | otherwise = 
        do 
            n <- integerToNat (x-1)
            Just (Succ n)


-- Small lib got maybe
 isJust :: Maybe a -> Bool
 isJust Nothing = False
 isJust _ = True

 isNothing :: Maybe a -> Bool
 isNothing = not . isJust

 mayybe :: b -> (a -> b) -> Maybe a -> b
 mayybe b _ Nothing = b
 mayybe _ f (Just a) = f a

 fromMaybe :: a -> Maybe a -> a
 fromMaybe = flip mayybe id 

 listToMaybe :: [a] -> Maybe a
 listToMaybe [] = Nothing
 listToMaybe (x:_) = Just x

 maybeToList :: Maybe a -> [a]
 maybeToList Nothing = []
 maybeToList (Just a) = [a]

 catMaybes :: [Maybe a] -> [a]
 catMaybes [] = []
 catMaybes (Nothing:xs) = catMaybes xs
 catMaybes (Just x :xs) = x : catMaybes xs

 flipMaybe :: [Maybe a] -> Maybe [a]
 flipMaybe [] = Just []
 flipMaybe (Nothing : xs) = Nothing
 flipMaybe (Just x : xs) = 
     do
         la <- flipMaybe xs
         Just (x:la)

 lefts' :: [Either a b] -> [a]
 lefts' = foldr f [] 
    where 
        f (Left x) xs = x:xs
        f _ xs = xs

 rights' :: [Either a b] -> [b] 
 rights' = foldr f [] 
    where 
        f (Right x) xs = x:xs
        f _ xs = xs

 partitionEithers' :: [Either a b] -> ([a], [b])
 partitionEithers' xs = (lefts' xs, rights' xs)

 eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
 eitherMaybe' _ (Left _) = Nothing
 eitherMaybe' f (Right b) = Just $ f b

 either' :: (a -> c) -> (b -> c) -> Either a b -> c
 either' f _ (Left a) = f(a)
 either' _ f (Right b) = f(b)

 eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
 eitherMaybe'' f = either' (\_ -> Nothing) (\b -> Just $ f b )

 