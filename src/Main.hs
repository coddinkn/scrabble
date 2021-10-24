module Main where

import App

import Control.Monad (void)
import System.Random (getStdGen)

import qualified Brick.Main as M

app :: M.App AppState e Name
app =
    M.App { M.appDraw = drawApp
          , M.appStartEvent = return
          , M.appHandleEvent = appEvent
          , M.appAttrMap = const attributeMap
          , M.appChooseCursor = M.showFirstCursor
          }

main :: IO ()
main = do
    appState <- newAppState <$> getStdGen
    (void . M.defaultMain app) appState
