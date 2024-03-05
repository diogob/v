{-# OPTIONS_GHC -Wno-orphans #-}

module HighlightedText
  ( HighlightedText (..),
    HighlightAttribute (..),
    highlightedZipper,
  )
where

import Brick.Widgets.Core (TextWidth (..))
import Brick.Widgets.Edit (DecodeUtf8 (..))
import Data.String (IsString (..))
import qualified Data.Text as T
import Data.Text.Zipper (TextZipper, mkZipper)
import Data.Text.Zipper.Generic
import qualified Graphics.Vty as V
import HighlightedText.Internal (HighlightAttribute (..), HighlightedText (..))
import qualified HighlightedText.Internal as H

instance TextWidth HighlightedText where
  textWidth = V.wcswidth . H.toList

instance GenericTextZipper HighlightedText where
  singleton = H.fromChar
  drop = H.drop
  take = H.take
  length = H.length
  last = H.last
  init = H.init
  null = H.null
  lines = H.lines
  toList = H.toList

instance DecodeUtf8 HighlightedText where
  decodeUtf8 bs = fromString . T.unpack <$> decodeUtf8 bs

highlightedZipper :: [HighlightedText] -> Maybe Int -> TextZipper HighlightedText
highlightedZipper =
  mkZipper H.fromChar H.drop H.take H.length H.last H.init H.null H.lines H.toList
