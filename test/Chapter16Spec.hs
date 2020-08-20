module Chapter16Spec (spec) where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Function
import Chapter16
import Data.Monoid

spec :: Spec
spec = do
  describe "functors" $ do
    it "should check identity for identity" $ do 
      quickCheck (functorIdentity :: Identity Int -> Bool)
    it "should check compose for identity" $ do 
      quickCheck (functorCompose :: (Identity Int) -> (Fun Int Int) -> (Fun Int Int) -> Bool)


functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => f a -> (Fun a b) -> (Fun b c)-> Bool
functorCompose x (Fun _ f) (Fun _ g) = (fmap (g . f) x) == (fmap g . fmap f $ x)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    x <- arbitrary
    return $ Pair x x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Two x y

instance ( Arbitrary a
         , Arbitrary b
         , Arbitrary c
         ) => Arbitrary (Three a b c) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Three x y z

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Three' x y y

instance ( Arbitrary a
         , Arbitrary b
         , Arbitrary c
         , Arbitrary d
         ) => Arbitrary (Four a b c d) where
  arbitrary = do
    w <- arbitrary
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Four w x y z

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Four' x x x y

instance Arbitrary a => Arbitrary (BoolAndSomethingElse a) where
  arbitrary = do
    x <- arbitrary
    elements [ False' x
             , True' x
             ]

instance Arbitrary a => Arbitrary (BoolAndMaybeSomethingElse a) where
  arbitrary = do
    x <- arbitrary
    elements [ Falsish
             , Truish x
             ]


