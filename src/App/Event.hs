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
    do maybeNewApp <-
        case app ^. status of
            Entering ->
                case event of

                    T.VtyEvent (V.EvKey V.KEnter []) ->
                        let editorContents = head . E.getEditContents $ app ^. userEnter
                            userListContents = L.listElements $ app ^. userList
                            newUserList = L.listInsert (maybe 0 (+ 1) $ L.listSelected (app ^. userList)) editorContents $ app ^. userList
                            invalidEntry = editorContents `Vec.elem` userListContents || editorContents == ""
                        in return . Just . Waiting $
                            if invalidEntry
                            then app & status .~ InvalidEntry
                            else app & status .~ NotEntering
                                     & userList .~ newUserList
                                     & userEnter .~ emptyUserEnter

                    T.VtyEvent (V.EvKey V.KEsc []) ->
                        return . Just . Waiting $ app & status .~ NotEntering

                    T.VtyEvent vtyEvent -> do
                        newUserEnter <- E.handleEditorEvent vtyEvent $ app ^. userEnter
                        return . Just . Waiting $ app & userEnter .~ newUserEnter

                    _ -> return . Just $ Waiting app

            NotEntering ->
                case event of

                    T.VtyEvent (V.EvKey (V.KChar '+') []) ->
                        return . Just . Waiting $ app & status .~ Entering

                    T.VtyEvent (V.EvKey V.KEsc []) ->
                        return Nothing

                    T.VtyEvent (V.EvKey V.KEnter []) ->
                        let players = L.listElements $ app ^. userList
                            numberOfPlayers = Vec.length players
                        in if numberOfPlayers >= 2 && numberOfPlayers <= 4
                           then either undefined (\(GS.InProgress inProgressState) -> return . Just . InProgress $ InProgressApp [] inProgressState) $
                                playScrabble [] (app ^. newGameState) $ do
                                     mapM_ addPlayer players
                                     mapM_ ready players
                           else return . Just . Waiting $ app & status .~ CantStart

                    T.VtyEvent _ -> do
                        newUserList <- userListEvent (app ^. userList) event
                        return . Just . Waiting $ app & userList .~ newUserList

                    _ -> return . Just $ Waiting app

            InvalidEntry ->
                case event of
                    T.VtyEvent (V.EvKey _ []) -> return . Just . Waiting $ app & status .~ Entering
                    _ -> return . Just $ Waiting app

            CantStart ->
                case event of
                    T.VtyEvent (V.EvKey _ []) -> return . Just . Waiting $ app & status .~ NotEntering
                    _ -> return . Just $ Waiting app

       case maybeNewApp of
           Just newApp -> M.continue newApp
           Nothing -> M.halt $ Waiting app

appEvent :: AppState -> T.BrickEvent Name e -> T.EventM Name (T.Next AppState)

appEvent (Waiting app) event = waitingEvent app event

appEvent app _ = M.halt app
