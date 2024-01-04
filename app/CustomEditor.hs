{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module CustomEditor (renderEditor) where

import Brick.Types
import Brick.Widgets.Core
import Brick.Widgets.Edit (Editor (..), editAttr, editContentsL, editFocusedAttr, getEditContents)
import qualified Data.Text.Zipper as Z hiding (textZipper)
import qualified Data.Text.Zipper.Generic as Z
import Lens.Micro

-- | Turn an editor state value into a widget. This uses the editor's
-- name for its scrollable viewport handle and the name is also used to
-- report mouse events.
renderEditor ::
  (Ord n, Show n, Monoid t, TextWidth t, Z.GenericTextZipper t, Show t) =>
  -- | The content drawing function
  ([t] -> Widget n) ->
  -- | Whether the editor has focus. It will report a cursor
  -- position if and only if it has focus.
  Bool ->
  -- | The editor.
  Editor t n ->
  Widget n
renderEditor draw foc editor =
  let cp = Z.cursorPosition z
      z = editor ^. editContentsL
      toTake = cp ^. _2
      toLeft = Z.take toTake (Z.currentLine z)
      cursorLoc = Location (textWidth toLeft, cp ^. _1)
      limit = maybe id vLimit (editor ^. editContentsL . to Z.getLineLimit)
      atChar = charAtCursor $ editor ^. editContentsL
      atCharWidth = maybe 1 textWidth atChar
   in withAttr (if foc then editFocusedAttr else editAttr) $
        limit $
          viewport (editorName editor) Both $
            (if foc then showCursor (editorName editor) cursorLoc else id) $
              visibleRegion cursorLoc (atCharWidth, 1) $
                draw $
                  getEditContents editor

-- private

charAtCursor :: (Z.GenericTextZipper t) => Z.TextZipper t -> Maybe t
charAtCursor z =
  let col = snd $ Z.cursorPosition z
      curLine = Z.currentLine z
      toRight = Z.drop col curLine
   in if Z.length toRight > 0
        then Just $ Z.take 1 toRight
        else Nothing
