module Main where

import Scrabble
import Scrabble.Board
import Scrabble.GameState
import Scrabble.Tile
import Scrabble.TilePlacement

import Control.Monad (void)
import Data.List (groupBy)
import qualified Graphics.Vty as V
import System.Random (getStdGen)

import qualified Brick.Types as T
import Brick.Types (Widget)
import qualified Brick.Main as M
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Core (str, (<=>))
import Brick.AttrMap (attrMap)

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

draw :: GameState -> [Widget ()]
draw state = pure . C.centerLayer . drawBoard $ board state

drawBoard :: Board -> Widget ()
drawBoard theBoard =
    let positions = (,) <$> [boardMin .. boardMax] <*> [boardMin .. boardMax]
        groupedPositions = groupBy (\a b -> fst a == fst b) positions
        tileStrs = map (\pos -> " " ++ showTile theBoard pos ++ " ") <$> groupedPositions
        v = "│"
        rows = map ((\row -> str $ v ++ row ++ v) . foldl1 (\a b -> a ++ v ++ b)) tileStrs
        h = str . concat $ ("├───" : replicate boardMax "┼───") ++ pure "┤"
        centerOfBoard = foldl1 (\a b -> a <=> h <=> b) rows
        top = str . concat $ ("┌───" : replicate boardMax "┬───") ++ pure "┐"
        bottom = str . concat $ ("└───" : replicate boardMax "┴───") ++ pure "┘"
    in top <=> centerOfBoard <=> bottom

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
    let dictionary = [ "BAT"
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
           $ playScrabble dictionary gameState scrabble
