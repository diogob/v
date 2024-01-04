module HighlightedText.InternalSpec (spec) where

import qualified HighlightedText.Internal as H
import Test.Hspec

spec :: Spec
spec = do
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
  describe "take" $ do
    it "get 5 initial elements" $ do
      H.take 5 "Hello World" `shouldBe` "Hello"
    it "get 4 initial elements" $ do
      H.take 4 (H.HighlightedText [(H.Body, "b"), (H.Body, "o"), (H.Body, "d"), (H.Body, "y")])
        `shouldBe` H.HighlightedText [(H.Body, "b"), (H.Body, "o"), (H.Body, "d"), (H.Body, "y")]
  describe "init" $ do
    it "get two initial elements" $ do
      H.init "123" `shouldBe` "12"
    it "return empty text when we have only one character" $ do
      H.init "1" `shouldBe` ""
    it "return event when there are empty contents" $
      do
        H.init (H.HighlightedText [(H.Title, "title"), (H.Body, "")]) `shouldBe` H.HighlightedText [(H.Title, "titl")]
  describe "toList" $ do
    it "converts contents to string" $ do
      H.toList "some highlighted text" `shouldBe` "some highlighted text"
