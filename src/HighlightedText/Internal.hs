{-# LANGUAGE NoImplicitPrelude #-}

module HighlightedText.Internal
  ( HighlightedText (..),
    HighlightAttribute (..),
    init,
    last,
    null,
    length,
    take,
    drop,
    lines,
    toList,
    fromChar,
  )
where

import Data.List (foldl', singleton)
import Data.String (IsString (..))
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
  (<>) (HighlightedText []) (HighlightedText htb) = HighlightedText $ trimHt htb
  (<>) (HighlightedText hta) (HighlightedText []) = HighlightedText $ trimHt hta
  (<>) (HighlightedText hta) (HighlightedText htb) = HighlightedText $ trimHt $ hta <> htb

instance Monoid HighlightedText where
  mempty = HighlightedText []

highlightList :: HighlightAttribute -> [String] -> [HighlightedText]
highlightList attribute body = HighlightedText . singleton . (,) attribute <$> body

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
  | P.length content < toTake = HighlightedText [(highlight, content)] <> take (toTake - P.length content) (HighlightedText tailH)
  | P.length content > toTake = HighlightedText [(highlight, P.take toTake content)]
  | otherwise = error "The guards above should cover all cases"

length :: HighlightedText -> Int
length (HighlightedText ht) = foldl' (\total (_, content) -> total + P.length content) 0 ht

last :: HighlightedText -> Char
last (HighlightedText ht) = (P.last . snd . P.last) ht

trimHt :: [(HighlightAttribute, String)] -> [(HighlightAttribute, String)]
trimHt = filter (\(_, content) -> not $ P.null content)

init :: HighlightedText -> HighlightedText
init (HighlightedText []) = HighlightedText []
init (HighlightedText ht)
  | P.null trimmedHt = HighlightedText []
  | otherwise = HighlightedText $ P.init trimmedHt <> [initLastPair $ P.last trimmedHt]
  where
    trimmedHt = trimHt ht
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
toList (HighlightedText ht) = foldl' (\str (_, content) -> str ++ content) [] ht
