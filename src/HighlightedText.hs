module HighlightedText where

import Data.List (singleton)

data HighlightAttribute = Title | Body
-- perhaps we should create a monoid instance for this and create a TextZipper over it
newtype HighlightedText = HighlightedText [(HighlightAttribute, String)]

withHighlightedTitleFromString :: [String] -> [HighlightedText]
withHighlightedTitleFromString =
  \case
    [] -> []
    (title : body) ->
      HighlightedText [(Title, title)] : (HighlightedText . singleton . (,) Body <$> body)

