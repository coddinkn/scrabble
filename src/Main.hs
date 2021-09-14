module Main where

import Draw

import Scrabble
import Scrabble.GameState
import Scrabble.Tile
import Scrabble.TilePlacement

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.List.NonEmpty (NonEmpty(..))
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

makeTilePlacement :: (Int, Int) -> Char -> TilePlacement
makeTilePlacement p l = flip TilePlacement p $ Tile l

scrabble :: Scrabble ()
scrabble = do
    addPlayer "dmr"
    addPlayer "knc"
    void . readyWithTiles "knc" $ map Tile "BATCATSBATCATS"
    void . readyWithTiles "dmr" $ map Tile "BATCATSBATCATS"
    void . placeTiles $ makeTilePlacement (7, 6) 'B' :|
                      [ makeTilePlacement (7, 7) 'A'
                      , makeTilePlacement (7, 8) 'T'
                      ]
    void . placeTiles $ makeTilePlacement (6, 5) 'A' :|
                      [ makeTilePlacement (6, 6) 'B' ]
    void . placeTiles $ makeTilePlacement (6, 7) 'C' :|
                      [ makeTilePlacement (8, 7) 'T' ]
    void . placeTiles $ makeTilePlacement (9, 7) 'S' :| []
    void . placeTiles $ makeTilePlacement (7, 9) 'S' :| []

appEvent :: GameState -> T.BrickEvent () e -> T.EventM () (T.Next GameState)

appEvent (Waiting state) event =
    case event of
        T.VtyEvent (V.EvKey (V.KChar 'a') []) ->
            case playScrabble dictionary (Waiting state) $ addPlayer "dmr" of
                Left e -> liftIO (print e) >> M.continue (Waiting state)
                Right newState -> M.continue newState

        T.VtyEvent (V.EvKey (V.KChar 'b') []) ->
            case playScrabble dictionary (Waiting state) scrabble of
                Left e -> liftIO (print e) >> M.continue (Waiting state)
                Right newState -> M.continue newState

        _ -> M.halt (Waiting state)

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
    (void . M.defaultMain app) gameState
