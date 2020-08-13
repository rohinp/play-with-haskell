module Chapter15Monoid where

import Data.Semigroup  (Semigroup, (<>))

data Trivial = Trivial
  deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Monoid Trivial where
  mempty  = Trivial

newtype Identity a = Identity a
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  Identity x <> Identity y = Identity (x <> y)

instance (Semigroup a, Monoid a) => Monoid (Identity a) where
  mempty = Identity mempty
  mappend = (<>)

data Two a b = Two a b
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  Two x y <> Two x' y' = Two (x <> x') (y <> y')

instance ( Semigroup a, Monoid a
         , Semigroup b, Monoid b
         ) => Monoid (Two a b) where
  mempty  = Two mempty mempty
  mappend = (<>)

newtype BoolConj = BoolConj Bool
  deriving (Eq, Show)

instance Semigroup BoolConj where
  BoolConj True  <> BoolConj True  = BoolConj True
  BoolConj False <> BoolConj _     = BoolConj False
  BoolConj True  <> BoolConj False = BoolConj False

instance Monoid BoolConj where
  mempty  = BoolConj True


newtype BoolDisj = BoolDisj Bool
  deriving (Eq, Show)

instance Semigroup BoolDisj where
  BoolDisj True  <> BoolDisj _     = BoolDisj True
  BoolDisj False <> BoolDisj True  = BoolDisj True
  BoolDisj False <> BoolDisj False = BoolDisj False

instance Monoid BoolDisj where
  mempty  = BoolDisj False

newtype Mem s a = Mem
                { runMem :: s -> (a, s) }

instance Semigroup a => Semigroup (Mem s a) where
  Mem f <> Mem g = Mem $ \s ->
    ( fst (f s) <> fst (g (snd (f s)))
    , snd $ g $ snd (f s)
    )

instance Monoid a => Monoid (Mem s a) where
  mempty  = Mem (\x -> (mempty, x))

f' :: Mem Int String
f' = Mem $ \s -> ("hi", s + 1)