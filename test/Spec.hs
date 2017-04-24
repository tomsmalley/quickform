{-# LANGUAGE DataKinds #-}

import Test.Hspec
import Test.QuickCheck
import QuickForm
import Example

main :: IO ()
main = hspec $ do
  describe "Some test" $ do

    it "test suite not implemented" $ do
      1 == 2 `shouldBe` True
