module Main where

import Draw

import Scrabble
import Scrabble.GameState
import Scrabble.Tile
import Scrabble.TilePlacement

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import qualified Graphics.Vty as V
import System.Random (getStdGen)

import qualified Brick.Types as T
import qualified Brick.Main as M
import Brick.AttrMap (attrMap)

dictionary :: [String]
dictionary = [ "BAT"
             , "CAT"
             , "CATS"
             , "BATS"
             , "AB"
             , "BB"
             , "ABC"
             ]

scrabble :: Scrabble ()
scrabble = do
    addPlayer "dmr"
    addPlayer "knc"
    void . readyWithTiles "knc" $ map Tile "BATCATSBATCATS"
    void . readyWithTiles "dmr" $ map Tile "BATCATSBATCATS"
    void $ placeTiles [ flip TilePlacement (7, 6) $ Tile 'B'
                      , flip TilePlacement (7, 7) $ Tile 'A'
                      , flip TilePlacement (7, 8) $ Tile 'T'
                      ]
    void $ placeTiles [ flip TilePlacement (6, 5) $ Tile 'A'
                      , flip TilePlacement (6, 6) $ Tile 'B'
                      ]
    void $ placeTiles [ flip TilePlacement (6, 7) $ Tile 'C'
                      , flip TilePlacement (8, 7) $ Tile 'T'
                      ]
    void $ placeTiles [ flip TilePlacement (9, 7) $ Tile 'S' ]
    void $ placeTiles [ flip TilePlacement (7, 9) $ Tile 'S' ]


appEvent :: GameState -> T.BrickEvent () e -> T.EventM () (T.Next GameState)

appEvent state (T.VtyEvent (V.EvKey (V.KChar 'a') []))
    | getStatus state == WaitingToStart =
        case playScrabble dictionary state $ addPlayer "dmr" of
            Left e -> liftIO (putStrLn e) >> M.continue state
            Right newState -> M.continue newState
    | otherwise = M.continue state

appEvent state _ = M.halt state

app :: M.App GameState e ()
app =
    M.App { M.appDraw = drawUI
          , M.appStartEvent = return
          , M.appHandleEvent = appEvent
          , M.appAttrMap = const $ attrMap V.defAttr []
          , M.appChooseCursor = M.neverShowCursor
          }

main :: IO ()
main = do
    gameState <- newGame <$> getStdGen 
    either putStrLn
           (void . M.defaultMain app)
           $ Right gameState
           -- $ playScrabble dictionary gameState scrabble
