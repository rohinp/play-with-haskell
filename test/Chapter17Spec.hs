module Chapter17Spec (spec) where

import Test.Hspec
import Test.QuickCheck hiding (Failure, Success)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Chapter17

spec :: Spec
spec = do
  describe "functors" $ do
    it "should check functor for list" $ do 
      quickBatch $ functor listAp
    it "should check applicative for list" $ do 
      quickBatch $ applicative listAp
    it "should check applicative for ziplist" $ do 
      quickBatch $ applicative $ ZipList' $  [('a', 'b', 'c')]
    it "should check applicative for validate" $ do 
      quickBatch $ applicative validAp


instance Arbitrary a => Arbitrary (List' a) where
  arbitrary = do
    x <- arbitrary
    return $ Cons x Nil

instance Eq a => EqProp (List' a) where
  (=-=) = eq

listAp :: List' (String, String, Int)
listAp = undefined

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where
      xs' = let (ZipList' l) = xs
            in take 3000 l
      ys' = let (ZipList' l) = ys
            in take 3000 l

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = ZipList' <$> arbitrary

instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    elements [ Failure x
             , Success y
             ]

instance (Eq e, Eq a) => EqProp (Validation e a) where
  (=-=) = eq

validAp :: Validation String (String, String, Int)
validAp = undefined

