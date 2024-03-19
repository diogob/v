import Control.DeepSeq
import Data.Foldable (Foldable (foldl'))
import Data.String (IsString (fromString))
import qualified Data.Text.Lazy.Zipper
import qualified Data.Text.Rope.Zipper as R
import Data.Text.Zipper (insertChar)
import qualified Data.Text.Zipper as Z
import qualified HighlightedText as H
import qualified HighlightedText.Internal as H
import Test.Tasty.Bench

testContent = "This is just a simple \ntest with 2 lines"

testHZipper = H.highlightedZipper [fromString testContent] Nothing

testTZipper = Z.textZipper [fromString testContent] Nothing

testSZipper = Z.stringZipper [fromString testContent] Nothing

testRZipper = fromString testContent :: R.RopeZipper

instance NFData R.RopeZipper

instance NFData Data.Text.Lazy.Zipper.TextZipper

insertChars :: (Monoid a) => [Char] -> Z.TextZipper a -> Z.TextZipper a
insertChars = flip $ foldl' (flip Z.insertChar)

insertRChars :: [Char] -> R.RopeZipper -> R.RopeZipper
insertRChars = flip $ foldl' (flip R.insertChar)

main :: IO ()
main =
  defaultMain
    [ bgroup
        "Identity"
        [ bench "HighlightedText" $ nf id testHZipper,
          bench "Text" $ nf id testTZipper,
          bench "Rope" $ nf id testRZipper,
          bench "String" $ nf id testSZipper
        ],
      bgroup
        "Append"
        [ bench "HighlightedText" $ nf (insertChars testContent) testHZipper,
          bench "Text" $ nf (insertChars testContent) testTZipper,
          bench "Rope" $ nf (insertRChars testContent) testRZipper,
          bench "String" $ nf (insertChars testContent) testSZipper
        ]
    ]
