module Chapter9Spec (spec) where

import Test.Hspec
import Chapter9

spec :: Spec
spec = do
  describe "chapter9 tests" $ do
    it "capitalizes first character of a string" $ do
      capitalize "what" `shouldBe` "What"
      capitalize "what" `shouldNotBe` "what"
    it "capitalizes first character of a string and returns that" $ do
      capitalizedHead "what" `shouldBe` 'W'

      