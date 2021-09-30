module App.Event
( appEvent
) where

import App.AppState
import App.Name

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
    do newApp <- if app ^. entering
                 then case event of
                     T.VtyEvent (V.EvKey V.KEnter []) ->
                        let contents = head . E.getEditContents $ app ^. userEnter
                            newUserList = L.listInsert (maybe 0 (+ 1) $ L.listSelected (app ^. userList)) contents $ app ^. userList
                        in return . Waiting $ app & userList  .~ newUserList
                                                  & userEnter .~ emptyUserEnter
                                                  & entering  .~ False
                     T.VtyEvent vtyEvent -> do
                        newUserEnter <- E.handleEditorEvent vtyEvent $ app ^. userEnter
                        return . Waiting $ app & userEnter .~ newUserEnter
                     _ -> return $ Waiting app
                 else case event of
                     T.VtyEvent (V.EvKey (V.KChar '+') []) ->
                        return . Waiting $ app & entering .~ True
                     T.VtyEvent _ -> do
                         newUserList <- waitingEvent (app ^. userList) event
                         return . Waiting $ app & userList .~ newUserList
                     _ -> return $ Waiting app
       M.continue newApp

appEvent app _ = M.halt app
