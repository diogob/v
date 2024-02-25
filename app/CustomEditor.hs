{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module CustomEditor (renderEditor, handleEditorEvent, editor, Editor, getEditContents, getCursorPosition) where

import Brick.AttrMap
import Brick.Types
import Brick.Widgets.Core
import Brick.Widgets.Edit (DecodeUtf8 (..))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.String (fromString)
import qualified Data.Text.Zipper as Z hiding (textZipper)
import qualified Data.Text.Zipper.Generic as Z
import qualified Data.Text.Zipper.Generic.Words as Z
import Data.Tuple (swap)
import Graphics.Vty (Event (..), Key (..), Modifier (..))
import HighlightedText (HighlightedText)
import qualified Parsers.MiniML.Lexer as MML
import qualified Parsers.MiniML.Parser as MML
import System.IO (hPrint, hPutStrLn, stderr)

data Editor t n = Editor
  { -- | The contents of the editor
    editContents :: Z.TextZipper t,
    -- | The name of the editor
    editorName :: n
  }

instance (Show t, Show n) => Show (Editor t n) where
  show e =
    concat
      [ "Editor { ",
        "editContents = " <> show (editContents e),
        ", editorName = " <> show (editorName e),
        "}"
      ]

instance Named (Editor t n) n where
  getName = editorName

-- | Construct an editor over 'String' values
editor ::
  (Z.GenericTextZipper a) =>
  -- | The editor's name (must be unique)
  n ->
  -- | The limit on the number of lines in the editor ('Nothing'
  -- means no limit)
  Maybe Int ->
  -- | The initial content
  a ->
  Editor a n
editor name limit s = Editor (Z.textZipper (Z.lines s) limit) name

-- | Apply an editing operation to the editor's contents.
--
-- This is subject to the restrictions of the underlying text zipper;
-- for example, if the underlying zipper has a line limit configured,
-- any edits applied here will be ignored if they edit text outside
-- the line limit.
applyEdit ::
  -- | The 'Z.TextZipper' editing transformation to apply
  (Z.TextZipper t -> Z.TextZipper t) ->
  Editor t n ->
  Editor t n
applyEdit f (Editor z name) =
  Editor (f z) name

-- | The attribute assigned to the editor when it does not have focus.
editAttr :: AttrName
editAttr = attrName "edit"

-- | The attribute assigned to the editor when it has focus. Extends
-- 'editAttr'.
editFocusedAttr :: AttrName
editFocusedAttr = editAttr <> attrName "focused"

-- | Get the contents of the editor.
getEditContents :: (Monoid t) => Editor t n -> [t]
getEditContents e = Z.getText $ editContents e

-- | Get the cursor position of the editor (row, column).
getCursorPosition :: Editor t n -> (Int, Int)
getCursorPosition e = Z.cursorPosition $ editContents e

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
  let z = editContents editor
      (row, col) = Z.cursorPosition z
      toLeft = Z.take col (Z.currentLine z)
      cursorLoc = Location (textWidth toLeft, row)
      limit = maybe id vLimit $ Z.getLineLimit z
      atChar = charAtCursor z
      atCharWidth = maybe 1 textWidth atChar
   in withAttr (if foc then editFocusedAttr else editAttr) $
        limit $
          viewport (editorName editor) Both $
            (if foc then showCursor (editorName editor) cursorLoc else id) $
              visibleRegion cursorLoc (atCharWidth, 1) $
                draw $
                  getEditContents editor

handleEditorEvent ::
  (Eq n) =>
  BrickEvent n e ->
  EventM n (Editor HighlightedText n) ()
handleEditorEvent e = do
  ed <- get
  let f = case e of
        VtyEvent ev ->
          handleVtyEvent ev
        MouseDown n _ _ (Location pos)
          | n == getName ed ->
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
  let newEd = applyEdit f ed
      allLines = Z.getText $ editContents newEd
      fullContent = unlines $ Z.toList <$> allLines
      ast = MML.runAlex (fromString fullContent) MML.parseMiniML
      tokens = MML.scanMany $ fromString fullContent
  liftIO $ hPutStrLn stderr "==============Tokens===================="
  liftIO $ hPrint stderr tokens
  liftIO $ hPutStrLn stderr "================AST====================="
  liftIO $ hPrint stderr ast
  liftIO $ hPutStrLn stderr "========================================"
  put newEd

-- private

charAtCursor :: (Z.GenericTextZipper t) => Z.TextZipper t -> Maybe t
charAtCursor z =
  let col = snd $ Z.cursorPosition z
      curLine = Z.currentLine z
      toRight = Z.drop col curLine
   in if Z.length toRight > 0
        then Just $ Z.take 1 toRight
        else Nothing
