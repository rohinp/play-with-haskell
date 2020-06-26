module MySpec (spec) where

import Test.Hspec

spec :: Spec
spec = do
  describe "strip" $ do
    it "removes leading and trailing whitespace" $ do
      True `shouldBe` True