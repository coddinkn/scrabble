module Main where

import Scrabble
import Scrabble.GameState
import Scrabble.Tile
import Scrabble.TilePlacement

import Control.Monad (void)
import qualified Graphics.Vty as V
import System.Random (getStdGen)

import qualified Brick.Types as T
import Brick.Types (Widget)
import qualified Brick.Main as M
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Core (str)
import Brick.AttrMap (attrMap)

scrabble :: Scrabble ()
scrabble = do
    addPlayer "dmr"
    addPlayer "knc"
    readyWithTiles "knc" $ map Tile "BATCATSBATCATS"
    readyWithTiles "dmr" $ map Tile "BATCATSBATCATS"
    placeTiles [ flip TilePlacement (7, 6) $ Tile 'B'
               , flip TilePlacement (7, 7) $ Tile 'A'
               , flip TilePlacement (7, 8) $ Tile 'T'
               ]
    placeTiles [ flip TilePlacement (6, 5) $ Tile 'A'
               , flip TilePlacement (6, 6) $ Tile 'B'
               ]
    placeTiles [ flip TilePlacement (6, 7) $ Tile 'C'
               , flip TilePlacement (8, 7) $ Tile 'T'
               ]
    placeTiles [ flip TilePlacement (9, 7) $ Tile 'S' ]
    void $ placeTiles [ flip TilePlacement (7, 9) $ Tile 'S' ]

draw :: GameState -> [Widget ()]
draw state = pure . C.centerLayer . B.border . str . show $ board state

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
main = do
    let words = [ "BAT"
                , "CAT"
                , "CATS"
                , "BATS"
                , "AB"
                , "BB"
                , "ABC"
                ]
    gameState <- newGame <$> getStdGen 
    either putStrLn
           (void . M.defaultMain app)
           $ playScrabble words gameState scrabble
