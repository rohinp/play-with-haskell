module Chapter15 where

import Data.Monoid

data Optional a = Nada | Only a deriving (Eq, Show)

instance Semigroup a => Semigroup (Optional a) where
    (<>) (Only x) Nada= Only x
    (<>) Nada (Only x)= Only x
    (<>) (Only x) (Only y)= Only (x <> y)
    (<>) _ _ = Nada

instance Monoid a => Monoid (Optional a) where
    mempty = Nada
    
type Adjective = String
type Adverb = String
type Noun = String
type Exclamation = String

madlibbin' :: Exclamation
    -> Adverb
    -> Noun
    -> Adjective
    -> String
madlibbin' e adv noun adj =
    e <> "! he said " <>
    adv <> " as he jumped into his car " <>
    noun <> " and drove off with his " <>
    adj <> " wife."

madlibbinBetter' :: Exclamation
    -> Adverb
    -> Noun
    -> Adjective
    -> String
madlibbinBetter' e adv noun adj = 
    mconcat [
        e, "! he said ",
        adv, " as he jumped into his car ",
        noun, " and drove off with his ",
        adj, " wife."]

newtype First' a = First' { getFirst' :: Optional a } deriving (Eq, Show)

instance Semigroup (First' a) where
    (<>) (First' { getFirst' = Only x}) (First' { getFirst' = Nada}) = 
        First' { getFirst' = Only x}
    (<>) (First' { getFirst' = Nada}) (First' { getFirst' = Only x}) = 
        First' { getFirst' = Only x}
    (<>) (First' { getFirst' = Only x}) (First' { getFirst' = Only y}) = 
        First' { getFirst' = Only x}
    (<>) _ _ = First' { getFirst' = Nada }

instance Monoid (First' a) where
    mempty = First' {getFirst' = Nada}

--Chapter exercises

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
    _ <> _ = Trivial

newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
    (Identity x) <> (Identity y) = Identity (x <> y)

data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
    (Two a1 b1) <> (Two a2 b2) = Two (a1 <> a2) (b1 <> b2)


data Three a b c = Three a b c
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c)
      => Semigroup (Three a b c) where
  Three x y z <> Three x' y' z' = Three (x <> x') (y <> y') (z <> z')

data Four a b c d = Four a b c d
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d)
      => Semigroup (Four a b c d) where
  Four w x y z <> Four w' x' y' z' =
    Four (w <> w') (x <> x') (y <> y') (z <> z')

newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  BoolConj True  <> BoolConj True  = BoolConj True
  BoolConj False <> BoolConj _     = BoolConj False
  BoolConj True  <> BoolConj False = BoolConj False

newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
  BoolDisj True  <> BoolDisj _     = BoolDisj True
  BoolDisj False <> BoolDisj True  = BoolDisj True
  BoolDisj False <> BoolDisj False = BoolDisj False

data Or a b = Fst a | Snd b deriving (Eq,Show)

instance Semigroup (Or a b) where
  Fst x <> Fst y = Fst y
  Fst x <> Snd y = Snd y
  Snd x <> Snd y = Snd x
  Snd x <> Fst y = Snd x


newtype Combine a b = Combine { unCombine :: (a -> b) }

instance Show (Combine a b) where
  show (Combine f) = "Combine (a -> b)"

instance Semigroup b => Semigroup (Combine a b) where
  Combine f <> Combine g = Combine (\x -> f x <> g x)

f :: Combine Integer (Sum Integer)
f = Combine $ \n -> Sum (n + 1)
g :: Combine Integer (Sum Integer)
g = Combine $ \n -> Sum (n - 1)

newtype Comp a = Comp { unComp :: (a -> a) }

instance Show (Comp a) where
  show (Comp f) = "Comp (a -> a)"

instance Eq (Comp a) where
   (Comp f) == (Comp f') = False

instance Semigroup (Comp a) where
  Comp f <> Comp g = Comp (f . g)


data Validation a b = Failure1 a
                    | Success1 b
                    deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
  Success1 x <> Failure1 y = Success1 x
  Failure1 x <> Failure1 y = Failure1 (x <> y)
  Success1 x <> Success1 y = Success1 x
  Failure1 x <> Success1 y = Success1 y

failure :: String -> Validation String Int
failure = Failure1

success :: Int -> Validation String Int
success = Success1