module App.Draw
( drawApp
) where

import App.AppState
import App.Name

import Scrabble.Board
import qualified Scrabble.GameState as GS

import Data.List (groupBy)

import Brick.Types (Widget)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.Edit as E
import Brick.Widgets.Core (str, (<+>), (<=>), vBox, hBox, hLimit, vLimit)

import Lens.Micro ((^.))

drawBoard :: Board -> Widget Name
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
        middle = topAndBottom <=> top <=> centerOfBoard <=> bottom <=> topAndBottom
    in hBox [ sides, middle, sides ]
    where letters = [ 'A' .. 'O' ]
          numbers = [ 1 .. 15 ] :: [Int]
          topAndBottom = hBox $ (\x -> str $ "  " ++ pure x ++ " ") <$> letters
          sides = str "\n" <=> vBox (str . (\x -> "\n " ++ show x ++ " ") <$> numbers)

waitingInstructions :: Widget Name
waitingInstructions = B.border $
                      vLimit 3 $ str "[+] - Add user\n[-] - Remove user\n[Esc] - Quit"

drawUserList :: UserList -> Widget Name
drawUserList list =
    B.borderWithLabel (str "Users") $
        hLimit 25 $
        vLimit 15 $
        L.renderList (\_ e -> str e) True list

drawUserEnter :: UserEnter -> Widget Name
drawUserEnter = C.centerLayer . B.border . hLimit 24 . E.renderEditor (str . concat) True

drawWaitingApp :: WaitingApp -> [Widget Name]
drawWaitingApp app =
    let userListWidget = C.center $ (drawUserList $ app ^. userList) <+> waitingInstructions
        userEnterWidget = drawUserEnter $ app ^. userEnter
    in  if app ^. entering
        then [ userEnterWidget, userListWidget ]
        else [ userListWidget ]

drawInProgress :: GS.InProgressState -> [Widget Name]
drawInProgress = pure . C.center . drawBoard . GS.getBoard

drawApp :: AppState -> [Widget Name]

drawApp (Waiting app) = drawWaitingApp app

drawApp (InProgress inProgressState) = drawInProgress inProgressState

drawApp _ = undefined
