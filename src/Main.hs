module Main where

import Scrabble.GameState

import Control.Monad (void)
import qualified Graphics.Vty as V

import qualified Brick.Types as T
import Brick.Types (Widget)
import qualified Brick.Main as M
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Core (str)
import Brick.AttrMap (attrMap)

draw :: GameState -> [Widget ()]
draw _ = pure . C.centerLayer . B.border $ str "Scrabble?"

appEvent :: GameState -> T.BrickEvent () e -> T.EventM () (T.Next GameState)
appEvent state _ = M.halt state

app :: M.App GameState e ()
app =
    M.App { M.appDraw = draw
          , M.appStartEvent = return
          , M.appHandleEvent = appEvent
          , M.appAttrMap = const $ attrMap V.defAttr []
          , M.appChooseCursor = M.neverShowCursor
          }

main :: IO ()
main = void . M.defaultMain app $ newGame 
