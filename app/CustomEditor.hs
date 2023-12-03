{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module CustomEditor (renderEditor) where

import Lens.Micro
import qualified Data.Text.Zipper as Z hiding ( textZipper )
import qualified Data.Text.Zipper.Generic as Z
import Brick.Widgets.Edit (Editor(..), editContentsL, editFocusedAttr, editAttr, getEditContents)
import Brick.Types
import Brick.Widgets.Core

-- | Turn an editor state value into a widget. This uses the editor's
-- name for its scrollable viewport handle and the name is also used to
-- report mouse events.
renderEditor :: (Ord n, Show n, Monoid t, TextWidth t, Z.GenericTextZipper t)
             => ([t] -> Widget n)
             -- ^ The content drawing function
             -> Bool
             -- ^ Whether the editor has focus. It will report a cursor
             -- position if and only if it has focus.
             -> Editor t n
             -- ^ The editor.
             -> Widget n
renderEditor draw foc e =
    let cp = Z.cursorPosition z
        z = e^.editContentsL
        toLeft = Z.take (cp^._2) (Z.currentLine z)
        cursorLoc = Location (textWidth toLeft, cp^._1)
        limit = case e^.editContentsL.to Z.getLineLimit of
            Nothing -> id
            Just lim -> vLimit lim
        atChar = charAtCursor $ e^.editContentsL
        atCharWidth = maybe 1 textWidth atChar
    in withAttr (if foc then editFocusedAttr else editAttr) $
       limit $
       viewport (editorName e) Both $
       (if foc then showCursor (editorName e) cursorLoc else id) $
       visibleRegion cursorLoc (atCharWidth, 1) $
       draw $
       getEditContents e

-- private


charAtCursor :: (Z.GenericTextZipper t) => Z.TextZipper t -> Maybe t
charAtCursor z =
    let col = snd $ Z.cursorPosition z
        curLine = Z.currentLine z
        toRight = Z.drop col curLine
    in if Z.length toRight > 0
       then Just $ Z.take 1 toRight
       else Nothing
