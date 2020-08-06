module AdditionSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Addition

spec :: Spec
spec = do
    describe "Addition" $ do 
        it "1 + 1 is greater than 1" $ do
            (1 + 1) > 1 `shouldBe` True
        it "2 + 2 is equal to 4" $ do
            2 + 2 `shouldBe` 4
        it "22 divided by 5 is 4 remainder 2" $ do
            dividedBy 22 5 `shouldBe` (4, 2)
        it "x is always greater than x" $ do
            property $ \x -> x + 1 > (x :: Int)
        it "multiply using recursive summation" $ do
            multiplies 20 10 `shouldBe` 200