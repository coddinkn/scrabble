module App.Event
( appEvent
) where

import App.AppState
import App.Name

import Scrabble
import qualified Scrabble.GameState as GS

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
                    in if numberOfPlayers >= 2 && numberOfPlayers <= 4
                       then either undefined (\(GS.InProgress inProgressState) -> M.continue . InProgress $ newInProgressApp [] inProgressState) $
                           playScrabble [] (app ^. newGameState) $ do
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

inProgressEvent :: InProgressApp -> T.BrickEvent Name e -> T.EventM Name (T.Next AppState)

inProgressEvent app event = do

    case app ^. inProgressStatus of

        _ -> case event of

            T.VtyEvent (V.EvKey V.KEsc []) -> M.halt $ InProgress app

            _ -> do newActionList <- actionListEvent (app ^. actionList) event
                    M.continue . InProgress $ app & actionList .~ newActionList

appEvent :: AppState -> T.BrickEvent Name e -> T.EventM Name (T.Next AppState)
appEvent (Waiting app) event = waitingEvent app event
appEvent (InProgress app) event = inProgressEvent app event
appEvent app _ = M.halt app
