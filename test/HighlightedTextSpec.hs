module HighlightedTextSpec (spec) where

import Test.Hspec

spec :: Spec
spec =
  describe "some function" $ do
    it "should be true" $ do
      True `shouldBe` True
