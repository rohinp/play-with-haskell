module Chapter18 where

import Control.Monad
import Control.Applicative
import Data.Monoid

--line number 735
bind :: Monad m => (a -> m b) -> m a -> m b
bind f = join . fmap f

--- page number 759

data Sum' a b = First' a | Second' b deriving (Eq, Show)

instance Functor (Sum' a) where
    fmap _ (First' a) = First' a
    fmap f (Second' b) = Second' (f b)

instance Applicative (Sum' a) where
    pure = Second'
    (<*>) (Second' f) (Second' a) = Second' (f a)
    (<*>) (First' a) _ = First' a
    (<*>) _ (First' a) = First' a


instance Monad (Sum' a) where
    return = pure
    (>>=) (First' a) _ = First' a
    (>>=) (Second' a) f = f a

data Nope a = NopeDotJpg

instance Functor Nope where
  fmap _ _ = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  _ <*> _ = NopeDotJpg

instance Monad Nope where
  return = pure
  _ >>= _ = NopeDotJpg

data BahEither b a = PLeft a | PRight b deriving (Show, Eq)

instance Functor (BahEither b) where
  fmap f (PLeft a)  = PLeft $ f a
  fmap _ (PRight b) = PRight b

instance Applicative (BahEither b) where
  pure = PLeft
  (PLeft f) <*> (PLeft a) = PLeft $ f a
  (PRight b) <*> _ = PRight b
  _ <*> (PRight b) = PRight b

instance Monad (BahEither b) where
  return = pure
  (PLeft a) >>= f = f a
  (PRight b) >>= _ = PRight b

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity a) = Identity $ f a

instance Monad Identity where
  return = pure
  (Identity a) >>= f = f a

data List' a = Nil' | Cons' a (List' a) deriving (Show, Eq)

append :: List' a -> List' a -> List' a
append Nil' ys         = ys
append (Cons' x xs) ys = Cons' x $ xs `append` ys

instance Functor List' where
  fmap _ Nil'         = Nil'
  fmap f (Cons' a as) = Cons' (f a) (fmap f as)

instance Applicative List' where
  pure a = Cons' a Nil'
  Nil' <*> _ = Nil'
  _ <*> Nil' = Nil'
  (Cons' f fs) <*> as = fmap f as `append` (fs <*> as)

instance Monad List' where
  return = pure
  Nil' >>= _ = Nil'
  (Cons' a as) >>= f = f a `append` (as >>= f)


j :: Monad m => m (m a) -> m a
j = join
{-
do
    a <- x 
    a
-}
{-
x >>= id
-}


l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 = liftA2

a :: Monad m => m a -> m (a -> b) -> m b
a = flip (<*>)

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
meh xs f = traverse f xs
-- sequence $ f <$> xs
--liftM2 (a1 -> a2 -> r) -> m a1 -> m a2 -> m r
--liftM2 (++) ((: []) <$> f x) (meh xs f)

flipType :: (Monad m) => [m a] -> m [a]
flipType xs = meh xs id

data Cow = Cow {
    name :: String
    , age :: Int
    , weight :: Int
    } deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative n | n >= 0 = Just n
             | otherwise = Nothing

weightCheck :: Cow -> Maybe Cow
weightCheck c =
    let w = weight c
        n = name c
    in if n == "Bess" && w > 499
        then Nothing
        else Just c
