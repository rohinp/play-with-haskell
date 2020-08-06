module MySpec (spec) where

import Test.Hspec
import qualified Data.Map as M
import Morse
import Test.QuickCheck


allowedChars :: [Char]
allowedChars = M.keys letterToMorse

allowedMorse :: [Morse]
allowedMorse = M.elems letterToMorse

charGen :: Gen Char
charGen = elements allowedChars

morseGen :: Gen Morse
morseGen = elements allowedMorse

prop_thereAndBackAgain :: Property
prop_thereAndBackAgain =
    forAll charGen
    (\c -> ((charToMorse c)
        >>= morseToChar) == Just c)

spec :: Spec
spec = do
  describe "morse" $ do
    it "there and back again" $ do
      prop_thereAndBackAgain