module App.Event
( appEvent
) where

import App.AppState
import App.Name

import qualified Data.Vector as Vec

import qualified Brick.Types as T
import qualified Brick.Main as M
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.Edit as E
import qualified Graphics.Vty as V

import Lens.Micro ((^.), (.~), (&))

waitingEvent :: UserList -> T.BrickEvent Name e -> T.EventM Name UserList

waitingEvent list (T.VtyEvent (V.EvKey (V.KChar '-') [])) =
    case L.listSelected list of
        Just i -> return $ L.listRemove i list
        Nothing -> return list

waitingEvent list (T.VtyEvent event) = L.handleListEvent event list

waitingEvent list _ = return list

appEvent :: AppState -> T.BrickEvent Name e -> T.EventM Name (T.Next AppState)

appEvent app (T.VtyEvent (V.EvKey V.KEsc [])) = M.halt app

appEvent (Waiting app) event =
    do newApp <-
        case app ^. enterState of
            Entering ->
                case event of
                    T.VtyEvent (V.EvKey V.KEnter []) ->
                        let editorContents = head . E.getEditContents $ app ^. userEnter
                            userListContents = L.listElements $ app ^. userList
                            newUserList = L.listInsert (maybe 0 (+ 1) $ L.listSelected (app ^. userList)) editorContents $ app ^. userList
                            invalidEntry = editorContents `Vec.elem` userListContents
                        in return . Waiting $ app & userList   .~ (if invalidEntry then app ^. userList  else newUserList)
                                                  & userEnter  .~ (if invalidEntry then app ^. userEnter else emptyUserEnter)
                                                  & enterState .~ (if invalidEntry then InvalidEntry     else NotEntering)
                    T.VtyEvent vtyEvent -> do
                        newUserEnter <- E.handleEditorEvent vtyEvent $ app ^. userEnter
                        return . Waiting $ app & userEnter .~ newUserEnter
                    _ -> return $ Waiting app
            NotEntering ->
                case event of
                    T.VtyEvent (V.EvKey (V.KChar '+') []) ->
                        return . Waiting $ app & enterState .~ Entering
                    T.VtyEvent _ -> do
                        newUserList <- waitingEvent (app ^. userList) event
                        return . Waiting $ app & userList .~ newUserList
                    _ -> return $ Waiting app
            InvalidEntry ->
                case event of
                    T.VtyEvent (V.EvKey _ []) -> return . Waiting $ app & enterState .~ Entering
                    _ -> return $ Waiting app
       M.continue newApp

appEvent app _ = M.halt app
