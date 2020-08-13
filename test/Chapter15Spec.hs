module Chapter15Spec (spec) where

import Test.Hspec
import Test.QuickCheck
import Chapter15
import Data.Monoid

spec :: Spec
spec = do
  describe "First' Optional Semigroup" $ do
    it "should check associativity" $ do
      quickCheck (monoidAssoc :: FirstMappend)
    it "should check left identity" $ do
      quickCheck (monoidLeftIdentity :: FstId)
    it "should check right identity" $ do
      quickCheck (monoidRightIdentity :: FstId)
  describe "Chapter exercises" $ do
      it "should check trivial semigroup associativity" $ do
        quickCheck (semigroupAssoc :: TrivAssoc)
      it "should check identity semigroup associativity" $ do
        quickCheck (semigroupAssoc :: StringIdentity)
      it "should check Two semigroup associativity" $ do
        quickCheck (semigroupAssoc :: TwoString)
      it "should check Three semigroup associativity" $ do
        quickCheck (semigroupAssoc :: ThreeAssoc)
      it "should check Four semigroup associativity" $ do
        quickCheck (semigroupAssoc :: FourAssoc)
      it "should boolconj 1" $ do
          ((BoolConj True) <> (BoolConj True)) == (BoolConj True)
      it "should boolconj 2" $ do
          ((BoolConj True) <> (BoolConj False)) == (BoolConj False)
      it "should boolDisj 1" $ do
          ((BoolDisj True) <> (BoolDisj True)) == (BoolDisj True)
      it "should boolDisj 2" $ do
          ((BoolDisj True) <> (BoolDisj False)) == (BoolDisj True)
      it "should Or 1" $ do
          (Fst 1) <> (Snd 2) `shouldBe` Snd 2
      it "should Or 2" $ do
          (((Fst 1) <> (Fst 2)) :: Or Int Int)  `shouldBe` Fst 2
      it "should Or 3" $ do
          (Snd 1) <> (Fst 2) `shouldBe` Snd 1
      it "should Or 4" $ do
          ((Snd 1) <> (Snd 2):: Or Int Int) `shouldBe` Snd 1
      it "should Combine 1" $ do
          (unCombine (f <> g) $ 0) `shouldBe` Sum {getSum = 0}
      it "should Combine 2" $ do
          (unCombine (f <> g) $ 1) `shouldBe` Sum {getSum = 2}
      it "should Combine 3" $ do
          (unCombine (f <> f) $ 1) `shouldBe` Sum {getSum = 4}
      it "should Combine 4" $ do
          (unCombine (g <> f) $ 1) `shouldBe` Sum {getSum = 2}
      it "should comp" $ do
          quickCheck (semigroupAssoc :: CompAssoc)
      it "should validation 1" $ do
          (success 1 <> failure "blah") `shouldBe` (Success1 1)
      it "should validation 2" $ do
          (failure "woot" <> failure "blah") `shouldBe` (Failure1 "wootblah")
      it "should validation 3" $ do
          (success 1 <> success 2) `shouldBe` (Success1 1)
      it "should validation 4" $ do
          (failure "woot" <> success 2) `shouldBe` (Success1 2)



semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

firstMappend :: First' a
    -> First' a
    -> First' a
firstMappend = mappend

type FirstMappend = First' String
    -> First' String
    -> First' String
    -> Bool

type FstId = First' String -> Bool

instance Arbitrary a => Arbitrary (First' a) where
    arbitrary = do
        a <- arbitrary
        frequency [
            (1, return (First' {getFirst' = Only a})),
            (1, return (First' {getFirst' = Nada}))
            ]

instance Arbitrary Trivial where
    arbitrary = return Trivial

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = do 
        a <- arbitrary
        return (Identity a)

type StringIdentity = Identity String -> Identity String -> Identity String -> Bool

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do 
        a <- arbitrary
        b <- arbitrary
        return (Two a b)

type TwoString = Two String String -> Two String String -> Two String String -> Bool

instance (Arbitrary a, Arbitrary b, Arbitrary c)
      => Arbitrary (Three a b c) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Three x y z

type ThreeAssoc = Three Trivial Trivial Trivial
               -> Three Trivial Trivial Trivial
               -> Three Trivial Trivial Trivial
               -> Bool

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d)
      => Arbitrary (Four a b c d) where
  arbitrary = do
    w <- arbitrary
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Four w x y z

type FourAssoc = Four Trivial Trivial Trivial Trivial
              -> Four Trivial Trivial Trivial Trivial
              -> Four Trivial Trivial Trivial Trivial
              -> Bool

instance Arbitrary BoolConj where
  arbitrary = elements [ BoolConj True
                       , BoolConj False
                       ]

type BoolConjAssoc = BoolConj
                  -> BoolConj
                  -> BoolConj
                  -> Bool


instance Arbitrary BoolDisj where
  arbitrary = elements [ BoolDisj True
                       , BoolDisj False
                       ]

type BoolDisjAssoc = BoolDisj
                  -> BoolDisj
                  -> BoolDisj
                  -> Bool

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    elements [ Fst x
             , Snd y
             ]

type OrAssoc = Or Trivial Trivial
            -> Or Trivial Trivial
            -> Or Trivial Trivial
            -> Bool



instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = do
    f <- arbitrary
    return $ Combine f

type CombAssoc = Combine Int (String -> String)
              -> Combine Int (String -> String)
              -> Combine Int (String -> String)
              -> Bool

instance (CoArbitrary a, Arbitrary a) => Arbitrary (Comp a) where
  arbitrary = do
    x <- arbitrary
    return $ Comp x

type CompAssoc = Comp Int
              -> Comp Int
              -> Comp Int
              -> Bool