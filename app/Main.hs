{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import qualified Brick as B
import qualified Brick.Widgets.Center as B
import Control.Monad (void)
import qualified CustomEditor as C
import Data.String (fromString)
import qualified Graphics.Vty as V
import HighlightedText
import Lens.Micro
import Lens.Micro.Mtl
import Lens.Micro.TH
import System.Environment (getArgs)

newtype State = State
  { _edit :: C.Editor C.Name
  }

makeLenses ''State

drawUI :: Int -> Int -> State -> [B.Widget C.Name]
drawUI displayLines displayColumns st = [ui]
  where
    e = C.renderWithLineNumbers (st ^. edit)
    ui = B.center $ B.hLimit displayColumns $ B.vLimit displayLines e

event :: B.BrickEvent C.Name e -> B.EventM C.Name State ()
event (B.VtyEvent (V.EvKey V.KEsc [])) =
  B.halt
event ev = do
  zoom edit $ C.handleEditorEvent ev

initialState :: HighlightedText -> State
initialState content =
  State (C.editor C.Edit Nothing content)

editorApp :: B.App State e C.Name
editorApp =
  B.App
    { B.appDraw = drawUI 100 200,
      B.appChooseCursor = const $ B.showCursorNamed C.Edit,
      B.appHandleEvent = event,
      B.appStartEvent = return (),
      B.appAttrMap = const C.vAttributes
    }

main :: IO ()
main = do
  args <- getArgs
  content <- case args of
    [path] -> readFile path
    _ -> pure ""
  void $ B.defaultMain editorApp $ initialState (fromString content)
