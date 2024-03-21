import Control.DeepSeq
import Data.Foldable (Foldable (foldl'))
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as N
import Data.String (IsString (fromString))
import qualified Data.Text.Lazy.Zipper
import qualified Data.Text.Rope.Zipper as R
import Data.Text.Zipper (insertChar)
import qualified Data.Text.Zipper as Z
import qualified Data.Vector as V
import HighlightedText (HighlightedText (HighlightedText))
import qualified HighlightedText as H
import qualified HighlightedText.Internal as H
import Test.Tasty.Bench

[testContent, testContent2x, testContent4x] = fmap produceContent [256, 512, 1024]
  where
    produceContent size = foldl' (<>) "" (N.take size infiniteContent)
    infiniteContent :: NonEmpty String
    infiniteContent = "This is just a simple \ntest with line breaks" :| repeat "This is just a simple \ntest with line breaks"

instance NFData R.RopeZipper

instance NFData Data.Text.Lazy.Zipper.TextZipper

insertChars :: (Monoid a) => [Char] -> Z.TextZipper a -> Z.TextZipper a
insertChars = flip $ foldl' (flip Z.insertChar)

insertRChars :: [Char] -> R.RopeZipper -> R.RopeZipper
insertRChars = flip $ foldl' (flip R.insertChar)

benchAll (highlightedBench, textBench, ropeBench, stringBench, vectorBench) =
  [ bench "HighlightedText" $ nf highlightedBench initialHZipper,
    bench "Text" $ nf textBench initialTZipper,
    bench "Rope" $ nf ropeBench initialRZipper,
    bench "String" $ nf stringBench initialSZipper,
    bench "Vector" $ nf vectorBench initialVZipper
  ]
  where
    initialHZipper = H.highlightedZipper [fromString testContent] Nothing
    initialTZipper = Z.textZipper [fromString testContent] Nothing
    initialSZipper = Z.stringZipper [fromString testContent] Nothing
    initialRZipper = fromString testContent :: R.RopeZipper
    initialVZipper = Z.vectorZipper [toVector testContent] Nothing
    toVector s = foldl' (<>) V.empty $ fmap V.singleton s

main :: IO ()
main =
  defaultMain
    [ bgroup
        "Identity"
        $ benchAll (id, id, id, id, id),
      bgroup
        "Append 1X"
        $ benchAll (insertChars testContent, insertChars testContent, insertRChars testContent, insertChars testContent, insertChars testContent),
      bgroup
        "Append 2X"
        $ benchAll (insertChars testContent2x, insertChars testContent2x, insertRChars testContent2x, insertChars testContent2x, insertChars testContent2x),
      bgroup
        "Append 4X"
        $ benchAll (insertChars testContent4x, insertChars testContent4x, insertRChars testContent4x, insertChars testContent4x, insertChars testContent4x),
      bgroup
        "gotoBOF"
        $ benchAll
          ( Z.gotoBOF,
            Z.gotoBOF,
            R.moveToLineStart . R.moveToFirstLine,
            Z.gotoBOF,
            Z.gotoBOF
          ),
      bgroup "Current Position" $ benchAll (Z.currentChar, Z.currentChar, R.cursor, Z.currentChar, Z.currentChar),
      bgroup "Current Position 4X" $
        benchAll
          ( Z.currentChar . insertChars testContent4x,
            Z.currentChar . insertChars testContent4x,
            R.cursor . insertRChars testContent4x,
            Z.currentChar . insertChars testContent4x,
            Z.currentChar . insertChars testContent4x
          )
    ]
