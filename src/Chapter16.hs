{-# LANGUAGE FlexibleInstances #-}

module Chapter16 where

import GHC.Arr
-- Exercises Page 625 Be Kind
-- f :: a -> a kind if a is *

--f :: a -> b a -> T (b a)
-- kind of b is * -> *
-- kind of T is * -> *

--f :: c a b -> c b a
-- Kind of c is * -> * -> *

{-
fmap1 :: (a -> b) -> fa -> fb
fmap2 :: (a -> b) -> fa -> fb

(.):: (b -> c) -> (a -> b) -> a -> c
        (.) ((a -> b) -> fa) -> ((a -> b) -> fa) -> 

-}

--Heavy lifting
a :: [Int]
a = (+1) <$> read "[1]" :: [Int]

b :: Maybe [String]
b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])

c :: Integer -> Integer
c = (*2) <$> (\x -> x - 2)

d :: Integer -> String
d = ((return '1' ++) . show) . (\x -> [x, 1..3])

e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        changed = fmap (read . ("123" ++) . show) ioi
    in (*3) <$> changed

-- QuickChecking Functor instances page 651


newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

data Pair a = Pair a a
instance Functor Pair where
  fmap f (Pair a1 a2) = Pair (f a1) (f a2)

data Two a b = Two a b
instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

data Three a b c = Three a b c
instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

data Three' a b = Three' a b b
instance Functor (Three' a) where
  fmap f (Three' x y y') = Three' x (f y) (f y')

data Four a b c d = Four a b c d
instance Functor (Four a b c) where
  fmap f (Four w x y z) = Four w x y (f z)


data Four' a b = Four' a a a b
instance Functor (Four' a) where
  fmap f (Four' x x' x'' y) = Four' x x' x'' (f y)

-- Can you implement one for this type?
data Trivial = Trivial

-- `Trivial has kind `*`
-- `Functor` requires kind `* -> *`

data Possibly a = LolNope | Yeppers a deriving (Eq, Show)
instance Functor Possibly where
  fmap _ LolNope     = LolNope
  fmap f (Yeppers x) = Yeppers (f x)

---Short exercise

data Sum a b = First a
             | Second b
             deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First x)  = First x
  fmap f (Second y) = Second (f y)

applyIfSecond :: (a -> b) -> (Sum e) a -> (Sum e) b
applyIfSecond = fmap

-----------------------------------------------------------------------------
-- 2.
-- Why is a `Functor` instance that applies the function only to `First`,
-- `Either`'s `Left`, impossible?
--
-- Answer:
-- A type constructor with more than two type arguments must include all
-- type arguments except the inner-most argument as part of the Functorial
-- structure being preserved.


--chapter exercise

-- Determine if the a valid `Functor` can be written theses datatypes.
--
-----------------------------------------------------------------------------
-- 1.
-- data Bool = False
--           | True
--
-- Answer:
-- Bool is a type constant with the kind `*`.

-----------------------------------------------------------------------------
-- 2.
data BoolAndSomethingElse a = False' a
                            | True' a
                            deriving (Eq, Show)

instance Functor BoolAndSomethingElse where
  fmap f (False' x) = False' (f x)
  fmap f (True' x)  = True' (f x)

-----------------------------------------------------------------------------
-- 3.
data BoolAndMaybeSomethingElse a = Falsish
                                 | Truish a
                                 deriving (Eq, Show)

instance Functor BoolAndMaybeSomethingElse where
  fmap _ Falsish    = Falsish
  fmap f (Truish x) = Truish (f x)


-----------------------------------------------------------------------------
-- 4.
newtype Mu f = InF
             { outF :: f (Mu f) }

-- Answer:
-- Mu has type `(* -> *) -> *` with `f` being a higher kinded type.


-----------------------------------------------------------------------------
-- 5.
data D = D (Array Word Word) Int Int
  deriving (Eq, Show)

-- Answer:
-- No

-- Rearrange the arguments of the type constuctor of the datatype so
-- the `Functor` instance works.
--
-----------------------------------------------------------------------------
-- 1.
data Sum' b a = First' a | Second' b

instance Functor (Sum' e) where
  fmap f (First' a)  = First' (f a)
  fmap f (Second' b) = Second' b

-----------------------------------------------------------------------------
-- 2.
data Company a c b = DeepBlue a c
                   | Something b

instance Functor (Company e e') where
  fmap f (Something b)  = Something (f b)
  fmap _ (DeepBlue a c) = DeepBlue a c

-----------------------------------------------------------------------------
-- 3.
data More b a = L a b a
              | R b a b
              deriving (Eq, Show)

instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'

  -- Write `Functor` instances for the following datatypes.
--
-----------------------------------------------------------------------------
-- 1.
data Quant a b = Finance
               | Desk a
               | Bloor b

instance Functor (Quant a) where
  fmap _ Finance   = Finance
  fmap _ (Desk x)  = Desk x
  fmap f (Bloor y) = Bloor (f y)

-----------------------------------------------------------------------------
-- 2.
data K a b = K a

instance Functor (K a) where
  fmap _ (K x)  = K x

-----------------------------------------------------------------------------
-- 3.
newtype Flip f a b = Flip (f b a)
  deriving (Eq, Show)

instance Functor (Flip K a) where
  fmap f (Flip (K a)) = Flip $ K (f a)

-----------------------------------------------------------------------------
-- 4.
data EvilGoateeConst a b = GoatyConst b

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst y) = GoatyConst (f y)

-----------------------------------------------------------------------------
-- 5.
data LiftItOut f a = LiftItOut (f a)

instance Functor f => Functor (LiftItOut f) where
  fmap f (LiftItOut fa) = LiftItOut (fmap f fa)

-----------------------------------------------------------------------------
-- 6.
data Parappa f g a = DaWrappa (f a) (g a)

instance (Functor f, Functor f1) => Functor (Parappa f f1) where
  fmap f (DaWrappa fa ga) = DaWrappa (fmap f fa) (fmap f ga)

-----------------------------------------------------------------------------
-- 7.
data IgnoreOne f g a b = IgnoringSomething (f a) (g b)

instance Functor g => Functor (IgnoreOne f g a) where
  fmap f (IgnoringSomething fa gb) = IgnoringSomething fa (fmap f gb)

-----------------------------------------------------------------------------
-- 8.
data Notorious g o a t = Notorious (g o) (g a) (g t)

instance Functor f => Functor (Notorious f a b) where
  fmap f (Notorious fx fy fz) = Notorious fx fy (fmap f fz)

-----------------------------------------------------------------------------
-- 9.
data List a = Nil
            | Cons a (List a)

instance Functor List where
  fmap _ Nil         = Nil
  fmap f (Cons x lx) = Cons (f x) (fmap f lx)

-----------------------------------------------------------------------------
-- 10.
data GoatLord a = NoGoat
                | OneGoat a
                | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)

instance Functor GoatLord where
  fmap _ NoGoat            = NoGoat
  fmap f (OneGoat x)       = OneGoat (f x)
  fmap f (MoreGoats x y z) = MoreGoats (fmap f x) (fmap f y) (fmap f z)

-----------------------------------------------------------------------------
-- 11.
data TalkToMe a = Halt
                | Print  String a
                | Read  (String -> a)

instance Functor TalkToMe where
  fmap _ Halt        = Halt
  fmap f (Print x y) = Print x (f y)
  fmap f (Read f2a)  = Read (fmap f f2a)