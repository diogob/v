module HighlightedTextSpec (spec) where

import Data.Text.Zipper
import HighlightedText (HighlightedText)
import qualified HighlightedText as H
import Test.Hspec

zipper :: TextZipper H.HighlightedText
zipper =
  H.highlightedZipper
    [ H.HighlightedText [(H.Title, "title")],
      H.HighlightedText [(H.Body, "body")]
    ]
    Nothing

emptyZipper :: TextZipper H.HighlightedText
emptyZipper =
  H.highlightedZipper
    [H.HighlightedText []]
    Nothing

insertA :: TextZipper HighlightedText -> TextZipper HighlightedText
insertA = insertChar 'a'

insertNewLine :: TextZipper HighlightedText -> TextZipper HighlightedText
insertNewLine = insertChar '\n'

spec :: Spec
spec = do
  describe "highlightedZipper" $ do
    it "should be able to clear zipper" $ do
      clearZipper zipper `shouldBe` emptyZipper
    it "should start on 1,4" $ do
      cursorPosition zipper `shouldBe` (1, 4)
    it "should be able to insert chars and update cursor" $ do
      cursorPosition (insertA $ insertA $ insertNewLine $ insertA $ insertA emptyZipper) `shouldBe` (1, 2)
