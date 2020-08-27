module Chapter17 where

import Control.Applicative
import Data.List (elemIndex)

--lookup 689

added :: Maybe Integer
added = (+ 3) <$> lookup 3 (zip [1, 2, 3] [4, 5, 6])
-------------

y :: Maybe Integer
y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

z :: Maybe Integer
z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z
------------------------

x' :: Maybe Int
x' = elemIndex 3 [1, 2, 3, 4, 5]

y' :: Maybe Int
y' = elemIndex 4 [1, 2, 3, 4, 5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' <$> x' <*> y'
--------------------------

xs :: [Integer]
xs = [1, 2, 3]

ys :: [Integer]
ys = [4, 5, 6]

x'' :: Maybe Integer
x'' = lookup 3 $ zip xs ys

y'' :: Maybe Integer
y'' = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed = sum <$> ((,) <$> x'' <*> y'')

---- Exercise: Identity instance 692

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
    pure = Identity
    (<*>) (Identity f) (Identity a) = Identity $ f a

--Exercise: Constant instance 693

newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Ord, Show)

instance Functor (Constant a) where
    fmap = undefined

instance Monoid a => Applicative (Constant a) where
    pure = undefined
    (<*>) = undefined


-- Exercise: Fixer upper

r1 :: Maybe String
r1 = const <$> Just "Hello" <*> pure "World"

r2 :: Maybe (Integer, Integer, String, [Integer])
r2 = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1, 2, 3]

---List' applicative exercise 719

data List' a = Nil | Cons a (List' a) deriving (Eq, Show)

instance Functor List' where
    fmap _ Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Semigroup (List' a) where
    (<>) Nil x = x
    (<>) (Cons a as) bs = Cons a (mappend as bs)

instance Monoid (List' a) where
    mempty = Nil
    mappend Nil x = x
    mappend (Cons a as) bs = Cons a (mappend as bs)

instance Applicative List' where
    pure x = Cons x Nil
    (<*>) Nil _ = Nil
    (<*>) _  Nil = Nil
    (<*>) (Cons f fs)  xs = mappend (fmap f xs) ((<*>) fs xs)

append :: List' a -> List' a -> List' a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List' a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List' (List' a) -> List' a
concat' = fold append Nil

flatMap :: (a -> List' b) -> List' a -> List' b
flatMap f = concat' . fmap f 


newtype ZipList' a = ZipList' [a] deriving (Eq, Show)

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure x                        = ZipList' $ pure x
  ZipList' _   <*> ZipList' [] = ZipList' []
  ZipList' [] <*> ZipList' _   = ZipList' []
  ZipList' fs  <*> ZipList' xs  = ZipList' $ fs <*> xs


data Validation err a = Failure err
                      | Success a
                      deriving (Eq, Show)

instance Functor (Validation e) where
  fmap _ (Failure e) = Failure e
  fmap f (Success x) = Success (f x)

instance Monoid e => Applicative (Validation e) where
  pure                    = Success
  Success f <*> Success x = Success (f x)
  Success _ <*> Failure y = Failure y
  Failure x <*> Success _ = Failure x
  Failure x <*> Failure y = Failure $ x <> y

--------chapter exercises 725

-- Given a type that has an instance of `Applicative`, specialize the types
-- of the methods.
--
-----------------------------------------------------------------------------
-- 1.
-- []
-- pure  :: a -> [a]
-- (<*>) :: [(a -> b)] -> [a] -> [b]
-- 
-- -----------------------------------------------------------------------------
-- -- 2.
-- -- IO
-- pure  :: a -> IO a
-- (<*>) :: IO (a -> b) -> IO a -> IO b
-- 
-- -----------------------------------------------------------------------------
-- -- 3.
-- -- (,) a
-- pure  :: Monoid a =>  b -> (a, b)
-- (<*>) :: Monoid c => (c, (a -> b)) -> (c, a) -> (c, b)
-- 
-- -----------------------------------------------------------------------------
-- -- 4.
-- -- (->) e
-- pure  :: a -> (e -> a)
-- (<*>) :: e -> (a -> b) -> (e -> a) -> (e -> b)


data Pair a = Pair a a
  deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair x x') = Pair (f x) (f x')

instance Applicative Pair where
  pure x = Pair x x
  Pair f f' <*> Pair x x' = Pair (f x) (f' x')

data Two a b = Two a b
  deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two x y) = Two x (f y)

instance Monoid a => Applicative (Two a) where
  pure = Two mempty
  Two x f <*> Two x' y = Two (x <> x') (f y)

data Three a b c = Three a b c
  deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure = Three mempty mempty
  Three x y f <*> Three x' y' z = Three (x <> x') (y <> y') (f z)

data Three' a b = Three' a b b
  deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' x y y') = Three' x (f y) (f y')

instance Monoid a => Applicative (Three' a) where
  pure x                           = Three' mempty x x
  Three' x f f' <*> Three' x' y y' = Three' (x <> x') (f y) (f' y')

data Four a b c d = Four a b c d
  deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four w x y z) = Four w x y (f z)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure = Four mempty mempty mempty
  Four x y z f <*> Four x' y' z' w = Four (x <> x') (y <> y') (z <> z') (f w)

data Four' a b = Four' a a a b
  deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' x x' x'' y) = Four' x x' x'' (f y)

instance Monoid a => Applicative (Four' a) where
  pure = Four' mempty mempty mempty
  Four' x x' x'' f <*> Four' y y' y'' z =
    Four' (x <> y) (x' <> y') (x'' <> y'') (f z)

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 (,,)