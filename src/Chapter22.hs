{-# LANGUAGE InstanceSigs #-}

module Chapter22 where

import Data.Char
import Control.Applicative
import Data.Maybe

boop :: Num a => a -> a
boop = (*2)

doop :: Num a => a -> a
doop = (+10)

bip :: Num a => a -> a
bip = boop . doop

bloop :: Num a => a -> a
bloop = fmap boop doop -- +10

bbop :: Integer -> Integer
bbop = (+) <$> boop <*> doop -- 

duwop :: Integer -> Integer
duwop = liftA2 (+) boop doop

--- line 850
cap :: String -> String
cap = map toUpper

rev :: String -> String
rev = reverse

composed :: String -> String
composed = cap . rev

fmapped :: String -> String
fmapped = cap <$> rev

tupled :: String -> (String, String)
tupled = (,) <$> cap <*> rev

tupled' :: String -> (String, String)
tupled' xx = (do
    x <- cap
    y <- rev
    return (x,y)) xx

tupled'' :: String -> (String, String)
tupled'' = cap >>= \c -> rev >>= \r -> return (c, r)

--page 855
newtype Reader r a =
    Reader { runReader :: r -> a }

ask :: Reader a a
ask = Reader id


 -- page 859

myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f fa fb = f <$> fa <*> fb

asks :: (r -> a) -> Reader r a
asks = Reader

instance Functor (Reader r) where
   fmap f (Reader ra) = Reader $ f . ra

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader $ const a

  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  (Reader rab) <*> (Reader ra) = Reader $ \r -> rab r $ ra r

instance Monad (Reader r) where
   return :: a -> Reader r a
   return = pure

   (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
   (Reader ra) >>= aRb = Reader $ \r -> runReader (aRb (ra r)) r

newtype HumanName = HumanName String deriving (Eq, Show)

newtype DogName = DogName String deriving (Eq, Show)

newtype Address = Address String deriving (Eq, Show)

data Person = Person {
      humanName :: HumanName
    , dogName :: DogName
    , address :: Address
  } deriving (Eq, Show)

data Dog = Dog {
      dogsName :: DogName
    , dogsAddress :: Address
  } deriving (Eq, Show)

getDogRM :: Reader Person Dog
getDogRM = do
  name <- asks dogName
  addy <- asks address
  return $ Dog name addy

-- chapter exercises page 867

x = [1, 2, 3]
y = [4, 5, 6]
z = [7, 8, 9]

xs :: Maybe Integer
xs = lookup 3 $ zip x y

ys :: Maybe Integer
ys = lookup 6 $ zip y z

zs :: Maybe Integer
zs = lookup 4 $ zip x y

z' :: Integer -> Maybe Integer
z' n = lookup n $ zip x z

x1 :: Maybe (Integer, Integer)
x1 = (,) <$> xs <*> ys

x2 :: Maybe (Integer, Integer)
x2 = (,) <$> ys <*> zs

x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 = (,) <$> z' <*> z'

summed :: Num c => (c, c) -> c
summed = uncurry (+)

bolt :: Integer -> Bool -- use &&, >3, <8
bolt = (&&) <$> (>3) <*> (<8)

sequA :: Integral a => a -> [Bool]
sequA = sequenceA [(> 3), (< 8), even]

s' :: Maybe Integer
s' = summed <$> ((,) <$> xs <*> ys)