module App.Event
( appEvent
) where

import App.AppState
import App.Name

import Scrabble
import Scrabble.TilePlacement (TilePlacement (..))
import qualified Scrabble.GameState as GS

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromJust)
import Data.List (find, delete)
import Data.List.NonEmpty (fromList)
import qualified Data.Vector as Vec

import qualified Brick.Types as T
import qualified Brick.Main as M
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.Edit as E
import qualified Graphics.Vty as V

import Lens.Micro ((^.), (.~), (&))

userListEvent :: UserList -> T.BrickEvent Name e -> T.EventM Name UserList

userListEvent list (T.VtyEvent (V.EvKey (V.KChar '-') [])) =
    case L.listSelected list of
        Just i -> return $ L.listRemove i list
        Nothing -> return list

userListEvent list (T.VtyEvent event) = L.handleListEvent event list

userListEvent list _ = return list

waitingEvent :: WaitingApp -> T.BrickEvent Name e -> T.EventM Name (T.Next AppState)

waitingEvent app event =
    case app ^. waitingStatus of
        Entering ->
            case event of

                T.VtyEvent (V.EvKey V.KEnter []) ->
                    let editorContents = head . E.getEditContents $ app ^. userEnter
                        userListContents = L.listElements $ app ^. userList
                        newUserList = L.listInsert (length userListContents) editorContents $ app ^. userList
                        invalidEntry = editorContents `Vec.elem` userListContents || editorContents == ""
                    in M.continue . Waiting $
                        if invalidEntry
                        then app & waitingStatus .~ InvalidEntry
                        else app & waitingStatus .~ NotEntering
                                 & userList .~ newUserList
                                 & userEnter .~ emptyUserEnter

                T.VtyEvent (V.EvKey V.KEsc []) ->
                    M.continue . Waiting $ app & waitingStatus .~ NotEntering

                T.VtyEvent vtyEvent -> do
                    newUserEnter <- E.handleEditorEvent vtyEvent $ app ^. userEnter
                    M.continue . Waiting $ app & userEnter .~ newUserEnter

                _ -> M.continue $ Waiting app

        NotEntering ->
            case event of

                T.VtyEvent (V.EvKey (V.KChar '+') []) ->
                    M.continue . Waiting $ app & waitingStatus .~ Entering

                T.VtyEvent (V.EvKey V.KEsc []) ->
                    M.halt $ Waiting app

                T.VtyEvent (V.EvKey V.KEnter []) ->
                    let players = L.listElements $ app ^. userList
                        numberOfPlayers = Vec.length players
                    in  do
                        dict <- liftIO $ lines <$> readFile "dictionary.txt"
                        if numberOfPlayers >= 2 && numberOfPlayers <= 4
                        then either undefined (\(GS.InProgress inProgressState) -> M.continue . InProgress $ newInProgressApp dict inProgressState) $
                            playScrabble dict (app ^. newGameState) $ do
                                mapM_ addPlayer players
                                mapM_ ready players
                        else M.continue . Waiting $ app & waitingStatus .~ CantStart

                T.VtyEvent _ -> do
                    newUserList <- userListEvent (app ^. userList) event
                    M.continue . Waiting $ app & userList .~ newUserList

                _ -> M.continue $ Waiting app

        InvalidEntry ->
            case event of
                T.VtyEvent (V.EvKey _ []) -> M.continue . Waiting $ app & waitingStatus .~ Entering
                _ -> M.continue $ Waiting app

        CantStart ->
            case event of
                T.VtyEvent (V.EvKey _ []) -> M.continue . Waiting $ app & waitingStatus .~ NotEntering
                _ -> M.continue $ Waiting app

actionListEvent :: ActionList -> T.BrickEvent Name e -> T.EventM Name ActionList
actionListEvent list (T.VtyEvent event) = L.handleListEvent event list
actionListEvent list _ = return list

tilesListEvent :: TilesList -> T.BrickEvent Name e -> T.EventM Name TilesList
tilesListEvent list (T.VtyEvent event) = L.handleListEvent event list
tilesListEvent list _ = return list

playTiles :: InProgressApp -> T.EventM Name (T.Next AppState)
playTiles app =
    let tps = app ^. tilePlacements
    in  if length tps > 1
        then case playScrabble (app ^. dictionary) (GS.InProgress $ app ^. gameState) . void $ placeTiles (fromList tps) of
                Right (GS.Waiting _) -> undefined
                Right (GS.InProgress inProgressGameState) -> M.continue . InProgress $ app & gameState .~ inProgressGameState
                Right (GS.Over _) -> M.halt Over
                Left exception -> M.continue . InProgress $ app & inProgressStatus .~ PlaceError (show exception)
        else M.continue . InProgress $ app & inProgressStatus .~ PlaceError "Must place more than one tile!"

inProgressEvent :: InProgressApp -> T.BrickEvent Name e -> T.EventM Name (T.Next AppState)

inProgressEvent app event = do

    case app ^. inProgressStatus of

        PlaceError _ -> case event of
            T.VtyEvent (V.EvKey _ []) -> M.continue . InProgress $ app & inProgressStatus .~ Menu
            _ -> M.continue $ InProgress app

        Menu -> case event of

            T.VtyEvent (V.EvKey V.KEsc []) -> M.halt $ InProgress app

            T.VtyEvent (V.EvKey V.KEnter []) ->
                let newStatus = snd . fromJust . L.listSelectedElement $ app ^. actionList
                in  case newStatus of

                        Pass -> case playScrabbleApp app pass of
                            Over -> M.halt Over
                            newApp -> M.continue newApp

                        Play -> playTiles app

                        _ -> M.continue . InProgress $ app & inProgressStatus .~ newStatus

            _ -> do newActionList <- actionListEvent (app ^. actionList) event
                    M.continue . InProgress $ app & actionList .~ newActionList

        Place pos placeStatus -> case event of

            T.VtyEvent (V.EvKey V.KEsc []) ->
                case placeStatus of
                    SelectingPosition -> M.continue . InProgress $ app & inProgressStatus .~ Menu
                    SelectingTile ->
                        let newPlaceStatus = Place pos SelectingPosition
                        in  M.continue . InProgress $ app & inProgressStatus .~ newPlaceStatus

            T.VtyEvent (V.EvKey V.KEnter []) -> M.continue . InProgress $
                case placeStatus of

                    SelectingPosition ->
                        let maybePlacedTile = find ((==) pos . position) $ app ^. tilePlacements
                            (newPlacements, newTilesList, newStatus) =
                                case maybePlacedTile of
                                    Just tilePlacement ->
                                        ( delete tilePlacement $ app ^. tilePlacements
                                        , L.listInsert 0 (tile tilePlacement) $ app ^. tilesList
                                        , SelectingPosition
                                        )
                                    Nothing -> (app ^. tilePlacements, app ^. tilesList, SelectingTile)
                        in app & inProgressStatus .~ Place pos newStatus
                               & tilePlacements .~ newPlacements
                               & tilesList .~ newTilesList

                    SelectingTile ->
                        let selectedTile = L.listSelectedElement $ app ^. tilesList
                            (newPlacements, newTilesList) =
                                case selectedTile of
                                    Just (i, t) -> (TilePlacement t pos : app ^. tilePlacements, L.listRemove i $ app ^. tilesList)
                                    Nothing -> (app ^. tilePlacements, app ^. tilesList)
                        in  app & inProgressStatus .~ Place pos SelectingPosition
                                & tilePlacements .~ newPlacements
                                & tilesList .~ newTilesList

            T.VtyEvent (V.EvKey key []) ->
                case placeStatus of

                    SelectingPosition ->
                        let (r, c) = pos
                            newPos = case key of
                                         V.KUp -> if r > 0 then (r - 1, c) else (r, c)
                                         V.KLeft ->  if c > 0 then (r, c - 1) else (r, c)
                                         V.KRight -> if c < 14 then (r, c + 1) else (r, c)
                                         V.KDown -> if r < 14 then (r + 1, c) else (r, c)
                                         _ -> (r, c)
                        in M.continue . InProgress $ app & inProgressStatus .~ Place newPos placeStatus

                    SelectingTile -> do newTilesList <- tilesListEvent (app ^. tilesList) event
                                        M.continue . InProgress $ app & tilesList .~ newTilesList

            _ -> M.continue $ InProgress app

        _ -> case event of
            T.VtyEvent (V.EvKey V.KEsc []) -> M.halt $ InProgress app
            _ -> M.continue $ InProgress app

appEvent :: AppState -> T.BrickEvent Name e -> T.EventM Name (T.Next AppState)
appEvent (Waiting app) event = waitingEvent app event
appEvent (InProgress app) event = inProgressEvent app event
appEvent app _ = M.halt app
