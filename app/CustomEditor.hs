{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module CustomEditor (renderEditor, handleEditorEvent) where

import Brick.Types
import Brick.Widgets.Core
import Brick.Widgets.Edit (Editor (..), editAttr, editContentsL, editFocusedAttr, getEditContents, applyEdit, DecodeUtf8 (..))
import qualified Data.Text.Zipper as Z hiding (textZipper)
import qualified Data.Text.Zipper.Generic as Z
import qualified Data.Text.Zipper.Generic.Words as Z
import Lens.Micro
import Graphics.Vty (Event(..), Key(..), Modifier(..))
import Data.Tuple (swap)

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

handleEditorEvent :: (Eq n, DecodeUtf8 t, Eq t, Z.GenericTextZipper t)
                  => BrickEvent n e
                  -> EventM n (Editor t n) ()
handleEditorEvent e = do
    ed <- get
    let f = case e of
              VtyEvent ev ->
                  handleVtyEvent ev
              MouseDown n _ _ (Location pos) | n == getName ed ->
                  Z.moveCursorClosest (swap pos)
              _anyOtherEvent -> id
        handleVtyEvent ev = case ev of
            EvPaste bs -> case decodeUtf8 bs of
                Left _ -> id
                Right t -> Z.insertMany t
            EvKey (KChar 'a') [MCtrl] -> Z.gotoBOL
            EvKey (KChar 'e') [MCtrl] -> Z.gotoEOL
            EvKey (KChar 'd') [MCtrl] -> Z.deleteChar
            EvKey (KChar 'd') [MMeta] -> Z.deleteWord
            EvKey (KChar 'k') [MCtrl] -> Z.killToEOL
            EvKey (KChar 'u') [MCtrl] -> Z.killToBOL
            EvKey KEnter [] -> Z.breakLine
            EvKey KDel [] -> Z.deleteChar
            EvKey (KChar c) [] | c /= '\t' -> Z.insertChar c
            EvKey KUp [] -> Z.moveUp
            EvKey KDown [] -> Z.moveDown
            EvKey KLeft [] -> Z.moveLeft
            EvKey KRight [] -> Z.moveRight
            EvKey (KChar 'b') [MCtrl] -> Z.moveLeft
            EvKey (KChar 'f') [MCtrl] -> Z.moveRight
            EvKey (KChar 'b') [MMeta] -> Z.moveWordLeft
            EvKey (KChar 'f') [MMeta] -> Z.moveWordRight
            EvKey KBS [] -> Z.deletePrevChar
            EvKey (KChar 't') [MCtrl] -> Z.transposeChars
            EvKey KHome [] -> Z.gotoBOL
            EvKey KEnd [] -> Z.gotoEOL
            EvKey (KChar '<') [MMeta] -> Z.gotoBOF
            EvKey (KChar '>') [MMeta] -> Z.gotoEOF
            _anyOtherKey -> id
    put $ applyEdit f ed
-- private

charAtCursor :: (Z.GenericTextZipper t) => Z.TextZipper t -> Maybe t
charAtCursor z =
  let col = snd $ Z.cursorPosition z
      curLine = Z.currentLine z
      toRight = Z.drop col curLine
   in if Z.length toRight > 0
        then Just $ Z.take 1 toRight
        else Nothing
