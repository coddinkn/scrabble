module Draw 
( drawUI
, drawBoard
) where

import Scrabble.Board
import Scrabble.GameState

import Data.List (groupBy)

import Brick.Types (Widget)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Core (str, (<=>), vBox)
import Lens.Micro ((^.))

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

drawGameState :: GameState -> Widget ()
drawGameState gameState =
    case gameState of
        Waiting state -> let u = state ^. users
                         in  B.border . vBox . map str $ u ++ pure "add player"
        InProgress state -> drawBoard $ getBoard state
        Over _ -> undefined

drawUI :: GameState -> [Widget ()]
drawUI = pure . C.centerLayer . drawGameState 

