{-# LANGUAGE NoImplicitPrelude #-}

module HighlightedText
  ( HighlightedText (..),
    HighlightAttribute (..),
    withHighlightedTitleFromString,
    highlightedZipper,
    init,
    last,
    null,
    length,
    take,
    drop,
    lines,
    toList,
  )
where

import Data.List (foldl', singleton)
import Data.String (IsString (..))
import Data.Text.Zipper (TextZipper, mkZipper)
import Prelude hiding (drop, init, last, length, lines, null, take)
import qualified Prelude as P

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
      HighlightedText [(Title, title)] : highlightList Body body

highlightList :: HighlightAttribute -> [String] -> [HighlightedText]
highlightList attribute body = HighlightedText . singleton . (,) attribute <$> body

-- Zipper functions
fromChar :: Char -> HighlightedText
fromChar c = fromString [c]

drop :: Int -> HighlightedText -> HighlightedText
drop _ (HighlightedText []) = HighlightedText []
drop toDrop (HighlightedText ((highlight, content) : tailH))
  | P.length content == toDrop = HighlightedText tailH
  | P.length content < toDrop = drop (P.length content - toDrop) $ HighlightedText tailH
  | P.length content > toDrop = HighlightedText ((highlight, P.drop toDrop content) : tailH)
  | otherwise = error "The guards above should cover all cases"

take :: Int -> HighlightedText -> HighlightedText
take _ (HighlightedText []) = HighlightedText []
take toTake (HighlightedText ((highlight, content) : tailH))
  | P.length content == toTake = HighlightedText [(highlight, content)]
  | P.length content < toTake = HighlightedText [(highlight, content)] <> take (P.length content - toTake) (HighlightedText tailH)
  | P.length content > toTake = HighlightedText [(highlight, P.take toTake content)]
  | otherwise = error "The guards above should cover all cases"

length :: HighlightedText -> Int
length (HighlightedText ht) = foldl' (\total (_, content) -> total + P.length content) 0 ht

last :: HighlightedText -> Char
last (HighlightedText ht) = (P.last . snd . P.last) ht

init :: HighlightedText -> HighlightedText
init (HighlightedText ht) = HighlightedText $ P.init ht <> [initLastPair $ P.last ht]
  where
    initLastPair (highlight, content) = (highlight, P.init content)

null :: HighlightedText -> Bool
null (HighlightedText ht) = P.null ht

lines :: HighlightedText -> [HighlightedText]
lines (HighlightedText ht) =
  initLines ++ case lastLine of
    [] -> []
    _ -> [HighlightedText lastLine]
  where
    (initLines, lastLine) = foldl' splitter ([], []) ht
    splitter (previousLines, currentLine) (highlight, content)
      | P.null splitLine = (previousLines, currentLine)
      | P.length splitLine == 1 = (previousLines, currentLine ++ ((,) highlight <$> splitLine))
      | P.length splitLine > 1 = (previousLines ++ highlightList highlight splitLine, [])
      | otherwise = error "The guards above should cover all cases"
      where
        splitLine = P.lines content

toList :: HighlightedText -> [Char]
toList (HighlightedText ht) = undefined

highlightedZipper :: [HighlightedText] -> Maybe Int -> TextZipper HighlightedText
highlightedZipper =
  mkZipper fromChar drop take length last init null lines toList
