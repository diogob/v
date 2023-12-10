module HighlightedText
  ( HighlightedText (..),
    HighlightAttribute (..),
    withHighlightedTitleFromString,
    highlightedZipper
  )
where

import Data.List (singleton, foldl')
import Data.String (IsString (..))
import Data.Text.Zipper (TextZipper, mkZipper)

data HighlightAttribute = Title | Body
  deriving (Show, Eq)

-- perhaps we should create a monoid instance for this and create a TextZipper over it
newtype HighlightedText = HighlightedText [(HighlightAttribute, String)]
  deriving (Show, Eq)

instance IsString HighlightedText where
  fromString = HighlightedText . singleton . (,) Body

instance Semigroup HighlightedText where
  (<>) (HighlightedText []) (HighlightedText htb) = HighlightedText htb
  (<>) (HighlightedText hta) (HighlightedText []) = HighlightedText hta
  (<>) (HighlightedText hta) (HighlightedText htb) = HighlightedText $ hta <> htb

instance Monoid HighlightedText where
  mempty = HighlightedText []

withHighlightedTitleFromString :: [String] -> [HighlightedText]
withHighlightedTitleFromString =
  \case
    [] -> []
    (title : body) ->
      HighlightedText [(Title, title)] : (HighlightedText . singleton . (,) Body <$> body)

-- Zipper functions
fromChar :: Char -> HighlightedText
fromChar c = fromString [c]

dropH :: Int -> HighlightedText -> HighlightedText
dropH _ (HighlightedText []) = HighlightedText []
dropH toDrop (HighlightedText ((highlight, content) : tailH))
  | length content == toDrop = HighlightedText tailH
  | length content < toDrop = dropH (length content - toDrop) $ HighlightedText tailH
  | length content > toDrop = HighlightedText ((highlight, drop toDrop content) : tailH)
  | otherwise = error "The guards above should cover all cases"

takeH :: Int -> HighlightedText -> HighlightedText
takeH _ (HighlightedText []) = HighlightedText []
takeH toTake (HighlightedText ((highlight, content) : tailH))
  | length content == toTake = HighlightedText [(highlight, content)]
  | length content < toTake = HighlightedText [(highlight, content)] <> takeH (length content - toTake) (HighlightedText tailH)
  | length content > toTake = HighlightedText [(highlight, take toTake content)]
  | otherwise = error "The guards above should cover all cases"

lengthH :: HighlightedText -> Int
lengthH (HighlightedText ht) = foldl' (\total (_,content) -> total + length content) 0 ht

lastH :: HighlightedText -> Char
lastH (HighlightedText ht) = (last . snd . last) ht

initH :: HighlightedText -> HighlightedText
initH (HighlightedText ht) = HighlightedText $ init ht <> [initLastPair $ last ht]
  where
    initLastPair (highlight, content) = (highlight, init content)

nullH :: HighlightedText -> Bool
nullH (HighlightedText ht) = null ht

-- linesH :: HighlightedText -> [HighlightedText]
-- linesH (HighlightedText ht) =
--   HighlightedText <$> undefined
--   where
--     contents = foldl' (\total (highlight,content) -> total <> ((,) highlight <$> lines content)) [] ht

-- highlightedZipper :: [HighlightedText] -> Maybe Int -> TextZipper HighlightedText
highlightedZipper =
  mkZipper fromChar dropH takeH lengthH lastH initH nullH
