{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module CustomEditor (renderEditor, handleEditorEvent, editor, Editor, getEditContents, getCursorPosition, Name (..), renderWithLineNumbers, lineNumberAttr, currentLineNumberAttr, vAttributes) where

import Brick.AttrMap
import Brick.Types
import Brick.Util (fg, on)
import Brick.Widgets.Core
import Brick.Widgets.Edit (DecodeUtf8 (..))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.String (fromString)
import qualified Data.Text.Zipper as Z hiding (textZipper)
import qualified Data.Text.Zipper.Generic as Z
import qualified Data.Text.Zipper.Generic.Words as Z
import Data.Tuple (swap)
import Data.Type.Equality (apply)
import qualified Debug.TimeStats as TS
import Graphics.Vty (Event (..), Key (..), Modifier (..))
import qualified Graphics.Vty as V
import HighlightedText (HighlightAttribute (Body, Title), HighlightedText (HighlightedText))
import qualified Parsers.MiniML.Lexer as MML
import qualified Parsers.MiniML.Parser as MML
import System.IO (hPrint, hPutStrLn, stderr)

data Name
  = Edit
  | EditLines
  deriving (Ord, Show, Eq)

data Editor n = Editor
  { -- | The contents of the editor
    editContents :: Z.TextZipper HighlightedText,
    -- | The name of the editor
    editorName :: n
  }

instance (Show n) => Show (Editor n) where
  show e =
    concat
      [ "Editor { ",
        "editContents = " <> show (editContents e),
        ", editorName = " <> show (editorName e),
        "}"
      ]

instance Named (Editor n) n where
  getName = editorName

vAttributes :: AttrMap
vAttributes =
  attrMap
    V.defAttr
    [ (editAttr, V.white `on` V.blue),
      (editFocusedAttr, V.black `on` V.linearColor (100 :: Integer) 100 100),
      (lineNumberAttr, fg V.cyan),
      (currentLineNumberAttr, V.defAttr `V.withStyle` V.bold),
      (attrName "title", fg V.red)
    ]

-- | Construct an editor over 'String' values
editor ::
  -- | The editor's name (must be unique)
  n ->
  -- | The limit on the number of lines in the editor ('Nothing'
  -- means no limit)
  Maybe Int ->
  -- | The initial content
  HighlightedText ->
  Editor n
editor name limit s = Editor (Z.textZipper (Z.lines s) limit) name

-- | Apply an editing operation to the editor's contents.
--
-- This is subject to the restrictions of the underlying text zipper;
-- for example, if the underlying zipper has a line limit configured,
-- any edits applied here will be ignored if they edit text outside
-- the line limit.
applyEdit ::
  -- | The 'Z.TextZipper' editing transformation to apply
  (Z.TextZipper HighlightedText -> Z.TextZipper HighlightedText) ->
  Editor n ->
  Editor n
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
getEditContents :: Editor n -> [HighlightedText]
getEditContents e = Z.getText $ editContents e

-- | Get the cursor position of the editor (row, column).
getCursorPosition :: Editor n -> (Int, Int)
getCursorPosition e = Z.cursorPosition $ editContents e

currentLineNumberAttr :: AttrName
currentLineNumberAttr = lineNumberAttr <> attrName "current"

lineNumberAttr :: AttrName
lineNumberAttr = attrName "lineNumber"

-- | Given an editor, render the editor with line numbers to the left of
-- the editor.
--
-- This essentially exploits knowledge of how the editor is implemented:
-- we make a viewport containing line numbers that is just as high as
-- the editor, then request that the line number associated with the
-- editor's current line position be made visible, thus scrolling it
-- into view. This is slightly brittle, however, because it relies on
-- essentially keeping the line number viewport and the editor viewport
-- in the same vertical scrolling state; with direct scrolling requests
-- from EventM it is easily possible to put the two viewports into a
-- state where they do not have the same vertical scrolling offset. That
-- means that visibility requests made with 'visible' won't necessarily
-- have the same effect in each viewport in that case. So this is
-- only really usable in the case where you're sure that the editor's
-- viewport and the line number viewports will not be managed by direct
-- viewport operations in EventB. That's what I'd recommend anyway, but
-- still, this is an important caveat.
--
-- There's another important caveat here: this particular implementation
-- has @O(n)@ performance for editor height @n@ because we generate
-- the entire list of line numbers on each rendering depending on the
-- height of the editor. That means that for sufficiently large files,
-- it will get more expensive to render the line numbers. There is a way
-- around this problem, which is to take the approach that the @List@
-- implementation takes: only render a region of visible line numbers
-- around the currently-edited line that is just large enough to be
-- guaranteed to fill the viewport, then translate that so that it
-- appears at the right viewport offset, thus faking a viewport filled
-- with line numbers when in fact we'd only ever render at most @2 * K +
-- 1@ line numbers for a viewport height of @K@. That's more involved,
-- so I didn't do it here, but that would be the way to go for a Real
-- Application.
renderWithLineNumbers :: Editor Name -> Widget Name
renderWithLineNumbers editor =
  TS.measurePure "Render with line numbers" $ lineNumbersVp <+> (TS.measurePure "Render editor VP" editorVp)
  where
    lineNumbersVp = hLimit (maxNumWidth + 1) $ viewport EditLines Vertical body
    drawText li = vBox $ highlightedLine <$> li
    editorVp = renderEditor drawText True editor
    body = withDefAttr lineNumberAttr $ vBox numWidgets
    numWidgets = mkNumWidget <$> numbers
    mkNumWidget i = maybeVisible i $ str $ show i
    maybeVisible i
      | i == curLine + 1 =
          visible . withDefAttr currentLineNumberAttr
      | otherwise =
          id
    numbers = [1 .. h]
    contents = getEditContents editor
    h = length contents
    curLine = fst $ getCursorPosition editor
    maxNumWidth = length $ show h

highlightedLine :: HighlightedText -> Widget n
highlightedLine (HighlightedText []) = str "\n"
highlightedLine (HighlightedText content) = hBox $ withHighlight <$> content

withHighlight :: (HighlightAttribute, String) -> Widget n
withHighlight (highlightAttribute, content) = withAttr (highlight highlightAttribute) (str content)
  where
    highlight =
      \case
        Title -> attrName "title"
        Body -> attrName "body"

-- name for its scrollable viewport handle and the name is also used to
-- report mouse events.
renderEditor ::
  (Ord n, Show n) =>
  -- | The content drawing function
  ([HighlightedText] -> Widget n) ->
  -- | Whether the editor has focus. It will report a cursor
  -- position if and only if it has focus.
  Bool ->
  -- | The editor.
  Editor n ->
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
  EventM n (Editor n) ()
handleEditorEvent e = TS.measureM "handleEditorEvent" $ do
  ed <- get
  let editFn = TS.measurePure "editFn" $ case e of
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
  let newEd = TS.measurePure "applyEdit" $ applyEdit editFn ed
      allLines = Z.getText $ editContents newEd
      fullContent = TS.measurePure "fullContent" $ unlines $ Z.toList <$> allLines
      ast = TS.measurePure "ast" $ MML.runAlex (fromString fullContent) MML.parseMiniML
      tokens = TS.measurePure "tokens" $ MML.scanMany $ fromString fullContent
  -- liftIO $ hPutStrLn stderr "==============Tokens===================="
  -- liftIO $ hPrint stderr tokens
  -- liftIO $ hPutStrLn stderr "================AST====================="
  -- liftIO $ hPrint stderr ast
  -- liftIO $ hPutStrLn stderr "========================================"
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
