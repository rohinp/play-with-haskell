module Chapter20 where

import Data.Foldable
import Data.Monoid
import Data.Semigroup (Sum(Sum))
import Data.Monoid (Product(getProduct))
import Data.Monoid (Product(Product))

--page number 818 Exercises: Library functions
sum' :: (Foldable t, Num a) => t a -> a
sum' = getSum . foldMap Sum 

product' :: (Foldable t, Num a) => t a -> a
product' = getProduct . foldMap Product 

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' a = getAny . foldMap (\x -> Any (x==a))

minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' = foldr f Nothing
    where
        f a Nothing = Just a 
        f a (Just v) | a <  v = Just a
                     | otherwise = Just v

maximum :: (Foldable t, Ord a) => t a -> Maybe a
maximum = foldr f Nothing
    where
        f a Nothing = Just a 
        f a (Just v) | a >  v = Just a
                     | otherwise = Just v

null' :: (Foldable t) => t a -> Bool
null' = getAll . foldMap (\_ -> All False)  

length' :: (Foldable t) => t a -> Int
length' = getSum . foldMap (\_ -> Sum 1)

toList' :: (Foldable t) => t a -> [a]
toList' = foldMap (: [])

fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap id

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr (\ a m -> m <> f a) mempty


-- chapter exercises 819

data Constant a b = Constant b

instance Foldable (Constant a) where
    foldMap f (Constant b) = f b

data Two a b = Two a b

instance Foldable (Two a) where
    foldMap f (Two _ b) = f b

data Three a b c = Three a b c

instance Foldable (Three a b) where
    foldMap f (Three _ _ c) = f c

data Three' a b = Three' a b b

instance Foldable (Three' a) where
    foldMap f (Three' _ l r) = f l <> f r

data Four' a b = Four' a b b b

instance Foldable (Four' a) where
    foldMap f (Four' _ l m r) = f l <> f m <> f r


filterF :: ( Applicative f
        , Foldable t
        , Monoid (f a))
        => (a -> Bool) -> t a -> f a
filterF pr = foldMap f
    where
        f a | pr a = pure a  
            | otherwise = mempty