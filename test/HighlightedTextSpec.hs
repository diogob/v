module HighlightedTextSpec (spec) where

import Test.Hspec
import qualified HighlightedText as H

spec :: Spec
spec =
  describe "lines" $ do
    it "empty list for empty content" $ do
      H.lines "" `shouldBe` []
    it "one element list for content without line breaks" $ do
      H.lines "one" `shouldBe` ["one"]
    it "two element list for content without line breaks" $ do
      H.lines "one\ntwo" `shouldBe` ["one", "two"]
    it "empty line for content with just a line break" $ do
      H.lines "\n" `shouldBe` [""]
    it "empty line for content with two line breaks" $ do
      H.lines "one\n\n" `shouldBe` ["one", ""]
    it "ignore trailing line breaks" $ do
      H.lines "one\ntwo\n" `shouldBe` ["one", "two"]
