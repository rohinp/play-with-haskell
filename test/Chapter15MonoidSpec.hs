module Chapter15MonoidSpec (spec) where

import Test.Hspec
import Test.QuickCheck hiding (Failure, Success)
import Chapter15Monoid
import Data.Monoid

monoidAssoc       :: (Eq m, Semigroup m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity   :: (Eq m, Semigroup m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity   :: (Eq m, Semigroup m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

instance Arbitrary Trivial where
  arbitrary = return Trivial

type TrivAssoc = 
    Trivial -> Trivial -> Trivial -> Bool

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    x <- arbitrary
    return $ Identity x

type IdAssoc = Identity String -> Identity String -> Identity String -> Bool
type IdId    = Identity String -> Bool

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Two x y

type TwoAssoc = Two Trivial Trivial
             -> Two Trivial Trivial
             -> Two Trivial Trivial
             -> Bool
type TwoId   = Two String String -> Bool

instance Arbitrary BoolConj where
  arbitrary = elements [ BoolConj True
                       , BoolConj False
                       ]

type BoolConjAssoc = BoolConj
                  -> BoolConj
                  -> BoolConj
                  -> Bool
type BoolConjId    = BoolConj -> Bool

instance Arbitrary BoolDisj where
  arbitrary = elements [ BoolDisj True
                       , BoolDisj False
                       ]

type BoolDisjAssoc = BoolDisj
                  -> BoolDisj
                  -> BoolDisj
                  -> Bool
type BoolDisjId    = BoolDisj -> Bool


spec :: Spec
spec = do
  describe "Chapter Exercises Monoid" $ do
    it "should check associativity Trivial" $ do
        quickCheck (monoidAssoc :: TrivAssoc)
    it "should check left Identity Trivial" $ do
        quickCheck (monoidLeftIdentity :: Trivial -> Bool)
    it "should check right Identity Trivial" $ do
        quickCheck (monoidRightIdentity :: Trivial -> Bool)
    it "should check associativity Identity" $ do
        quickCheck (monoidAssoc :: IdAssoc)
    it "should check left Identity Identity" $ do
        quickCheck (monoidLeftIdentity :: IdId )
    it "should check right Identity Identity" $ do
        quickCheck (monoidRightIdentity :: IdId)
    it "should check associativity Two" $ do
        quickCheck (monoidAssoc :: TwoAssoc)
    it "should check left Identity Two" $ do
        quickCheck (monoidLeftIdentity :: TwoId )
    it "should check right Identity Two" $ do
        quickCheck (monoidRightIdentity :: TwoId)
    it "should check associativity BoolConj" $ do
        quickCheck (monoidAssoc :: BoolConjAssoc)
    it "should check left Identity BoolConj" $ do
        quickCheck (monoidLeftIdentity :: BoolConjId )
    it "should check right Identity BoolConj" $ do
        quickCheck (monoidRightIdentity :: BoolConjId)
    it "should check associativity BoolDisj" $ do
        quickCheck (monoidAssoc :: BoolDisjAssoc)
    it "should check left Identity BoolDisj" $ do
        quickCheck (monoidLeftIdentity :: BoolDisjId )
    it "should check right Identity BoolDisj" $ do
        quickCheck (monoidRightIdentity :: BoolDisjId)