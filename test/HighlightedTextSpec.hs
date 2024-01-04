module HighlightedTextSpec (spec) where

import Brick.Widgets.Core (TextWidth (..))
import Data.Text.Zipper
import qualified Data.Text.Zipper.Generic as Z
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

weirdBodyZipper :: TextZipper H.HighlightedText
weirdBodyZipper =
  H.highlightedZipper
    [ H.HighlightedText [(H.Body, "b"), (H.Body, "o"), (H.Body, "d"), (H.Body, "y")]
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
  describe "textWidth" $ do
    it "return 0 for empty text" $ do
      textWidth (H.HighlightedText []) `shouldBe` 0
    it "return 4 for text with 4 characters" $ do
      textWidth (Z.take 4 (currentLine weirdBodyZipper)) `shouldBe` 4
  describe "highlightedZipper" $ do
    it "should be able to clear zipper" $ do
      clearZipper zipper `shouldBe` emptyZipper
    it "should get cursor position" $ do
      cursorPosition zipper `shouldBe` (1, 4)
    it "should get cursor position 2" $ do
      cursorPosition weirdBodyZipper `shouldBe` (0, 4)
    it "should be able to insert chars and update cursor" $ do
      cursorPosition (insertA $ insertA $ insertNewLine $ insertA $ insertA emptyZipper) `shouldBe` (1, 2)
