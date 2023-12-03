{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import qualified Brick.AttrMap as A
import qualified Brick.Main as M
import qualified Brick.Types as T
import Brick.Util (fg, on)
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Core
  ( hLimit,
    str,
    vBox,
    vLimit,
    viewport,
    visible,
    withDefAttr,
    (<+>),
  )
import qualified Brick.Widgets.Edit as E
import Control.Monad (void)
import qualified Graphics.Vty as V
import Lens.Micro
import Lens.Micro.Mtl
import Lens.Micro.TH
import System.Environment (getArgs)

data Name
  = Edit
  | EditLines
  deriving (Ord, Show, Eq)

newtype St = St
  { _edit :: E.Editor String Name
  }

makeLenses ''St

drawUI :: Int -> Int -> St -> [T.Widget Name]
drawUI displayLines displayColumns st = [ui]
  where
    e = renderWithLineNumbers (st ^. edit)
    ui = C.center $ hLimit displayColumns $ vLimit displayLines e

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
-- viewport operations in EventM. That's what I'd recommend anyway, but
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
renderWithLineNumbers :: E.Editor String Name -> T.Widget Name
renderWithLineNumbers e =
  lineNumbersVp <+> editorVp
  where
    lineNumbersVp = hLimit (maxNumWidth + 1) $ viewport EditLines T.Vertical body
    editorVp = E.renderEditor (str . unlines) True e
    body = withDefAttr lineNumberAttr $ vBox numWidgets
    numWidgets = mkNumWidget <$> numbers
    mkNumWidget i = maybeVisible i $ str $ show i
    maybeVisible i
      | i == curLine + 1 =
          visible . withDefAttr currentLineNumberAttr
      | otherwise =
          id
    numbers = [1 .. h]
    contents = E.getEditContents e
    h = length contents
    curLine = fst $ E.getCursorPosition e
    maxNumWidth = length $ show h

event :: T.BrickEvent Name e -> T.EventM Name St ()
event (T.VtyEvent (V.EvKey V.KEsc [])) =
  M.halt
event ev = do
  zoom edit $ E.handleEditorEvent ev

initialState :: String -> St
initialState content =
  St (E.editor Edit Nothing content)

lineNumberAttr :: A.AttrName
lineNumberAttr = A.attrName "lineNumber"

currentLineNumberAttr :: A.AttrName
currentLineNumberAttr = lineNumberAttr <> A.attrName "current"

vAttributes :: A.AttrMap
vAttributes =
  A.attrMap
    V.defAttr
    [ (E.editAttr, V.white `on` V.blue),
      (E.editFocusedAttr, V.black `on` V.yellow),
      (lineNumberAttr, fg V.cyan),
      (currentLineNumberAttr, V.defAttr `V.withStyle` V.bold)
    ]

vEditor :: M.App St e Name
vEditor =
  M.App
    { M.appDraw = drawUI 100 200,
      M.appChooseCursor = const $ M.showCursorNamed Edit,
      M.appHandleEvent = event,
      M.appStartEvent = return (),
      M.appAttrMap = const vAttributes
    }

main :: IO ()
main = do
  args <- getArgs
  content <- case args of
        [path] -> readFile path
        _ -> pure ""
  void $ M.defaultMain vEditor $ initialState content
