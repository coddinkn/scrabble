module App.Draw
( drawApp
) where

import App.AppState
import App.Attributes
import App.Name

import Scrabble.Board
import Scrabble.Tile (Tile)
import Scrabble.Modifier
import qualified Scrabble.GameState as GS

import Data.List (groupBy)

import Brick.Types (Widget)
import Brick.AttrMap (AttrName)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.Edit as E
import Brick.Widgets.Core (str, (<+>), (<=>), vBox, hBox, hLimit, vLimit, withAttr)

import Lens.Micro ((^.))

boardPositionAttr :: Position -> AttrName
boardPositionAttr pos =
    case modifier pos of
                     Just DoubleLetter -> doubleLetterAttr
                     Just TripleLetter -> tripleLetterAttr
                     Just DoubleWord   -> doubleWordAttr
                     Just TripleWord   -> tripleWordAttr
                     Nothing           -> if pos == middle
                                          then doubleWordAttr
                                          else defaultAttr

drawPosition :: InProgressApp -> Position -> Widget Name
drawPosition app pos = withAttr positionAttr . str $ " " ++ showTile board pos ++ " "
    where board = GS.getBoard $ app ^. gameState
          positionAttr =
              case app ^. inProgressStatus of
                  Place selectedPosition _ placeStatus ->
                    if selectedPosition == pos
                    then case placeStatus of
                            SelectingPosition -> selectedForPlacingFocusedAttr
                            SelectingTile -> selectedForPlacingAttr
                    else boardPositionAttr pos
                  _ -> boardPositionAttr pos

drawBoard :: InProgressApp -> Widget Name
drawBoard app =
    let positions = (,) <$> [boardMin .. boardMax] <*> [boardMin .. boardMax]
        groupedPositions = groupBy (\a b -> fst a == fst b) positions
        tileWidgets = map (drawPosition app) <$> groupedPositions
        v = "│"
        rows = map ((\row -> str v <+> row <+> str v) . foldl1 (\a b -> a <+> str v <+> b)) tileWidgets
        h = str . concat $ ("├───" : replicate boardMax "┼───") ++ pure "┤"
        centerOfBoard = foldl1 (\a b -> a <=> h <=> b) rows
        top = str . concat $ ("┌───" : replicate boardMax "┬───") ++ pure "┐"
        bottom = str . concat $ ("└───" : replicate boardMax "┴───") ++ pure "┘"
        boardMiddle = topAndBottom <=> top <=> centerOfBoard <=> bottom <=> topAndBottom
    in hBox [ sides, boardMiddle, sides ]
    where letters = [ 'A' .. 'O' ]
          numbers = [ 1 .. 15 ] :: [Int]
          topAndBottom = hBox $ (\x -> str $ "  " ++ pure x ++ " ") <$> letters
          sides = str "\n" <=> vBox (str . (\x -> "\n " ++ show x ++ " ") <$> numbers)

waitingInstructions :: Widget Name
waitingInstructions = B.borderWithLabel (str "Controls") $
                      vLimit 4 $ str "[+] - Add user\n[-] - Remove user\n[Esc] - Quit\n[Enter] - Start game"

invalidEntryNotification :: Widget Name
invalidEntryNotification = C.centerLayer . B.borderWithLabel (str "Invalid Entry") $ str "[ press any key to continue ]"

cantStartNotification :: Widget Name
cantStartNotification = C.centerLayer . B.borderWithLabel (str "Can't Start Game") $ str "    2-4 players required\n[ press any key to continue ]"

drawUserList :: UserList -> Widget Name
drawUserList list =
    B.borderWithLabel (str "Users") $
        hLimit 25 $
        vLimit 15 $
        L.renderList (\_ e -> str e) True list

drawUserEnter :: UserEnter -> Widget Name
drawUserEnter = C.centerLayer . B.borderWithLabel (str "Enter username") . hLimit 24 . E.renderEditor (str . concat) True

drawWaitingApp :: WaitingApp -> [Widget Name]
drawWaitingApp app =
    let userListWidget = C.center $ drawUserList (app ^. userList) <+> waitingInstructions
        userEnterWidget = drawUserEnter $ app ^. userEnter
    in  case app ^. waitingStatus of
        CantStart -> [ cantStartNotification, userListWidget ]
        InvalidEntry -> [ invalidEntryNotification, userListWidget ]
        Entering -> [ userEnterWidget, userListWidget ]
        NotEntering -> [ userListWidget ]

drawWhosTurn :: InProgressApp -> Widget Name
drawWhosTurn app = B.borderWithLabel (str "Current turn") . hLimit 24 . C.hCenter . str . GS.whosTurn $ app ^. gameState

drawActionList :: InProgressStatus -> ActionList -> Widget Name
drawActionList status = B.borderWithLabel (str "Actions") . vLimit 3 . hLimit 8 . L.renderList (\_ e -> str $ show e) focused
    where focused = status == Menu

drawTile :: Tile -> Widget Name
drawTile = B.border . str . spaceOut . show
    where spaceOut tileStr = " " ++ tileStr ++ " "

drawTilesList :: InProgressStatus -> TilesList -> Widget Name
drawTilesList status = B.borderWithLabel (str "Tiles") . vLimit 21 . hLimit 5 . L.renderList (\_ e -> drawTile e) focused
    where focused = case status of
                         Place _ _ SelectingTile -> True
                         _ -> False

drawInProgressApp :: InProgressApp -> [Widget Name]
drawInProgressApp app =
    let board = B.border . drawBoard $ app
        currentTurn = drawWhosTurn app
        actions = drawActionList (app ^. inProgressStatus) $ app ^. actionList
        tiles = drawTilesList (app ^. inProgressStatus) $ app ^. tilesList
    in pure . C.vCenter $ C.hCenter currentTurn <=> C.hCenter (board <+> (actions <=> tiles))

drawApp :: AppState -> [Widget Name]
drawApp (Waiting app) = drawWaitingApp app
drawApp (InProgress inProgressApp) = drawInProgressApp inProgressApp
drawApp _ = undefined
