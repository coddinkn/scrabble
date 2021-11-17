module App.AppState
( AppState(..)
, WaitingApp(..)
, InProgressApp(..)
, newAppState
, userList
, userEnter
, waitingStatus
, dictionary
, gameState
, newGameState
, newInProgressApp
, ActionList
, inProgressStatus
, actionList
, UserList
, UserEnter
, emptyUserEnter
, WaitingStatus(..)
, InProgressStatus(..)
) where

import App.Name (Name)
import qualified App.Name as Name

import Scrabble.GameState ( Username
                          , GameState
                          , InProgressState
                          , newGame
                          )
import Scrabble.Position (Position)
import Scrabble.TilePlacement (TilePlacement)

import Control.Monad.Random (StdGen)
import Data.Vector (empty, fromList)

import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.Edit as E

import Lens.Micro.TH

type UserList = L.List Name Username

type UserEnter = E.Editor Username Name

data WaitingStatus = Entering
                   | NotEntering
                   | InvalidEntry
                   | CantStart

data WaitingApp =
    WaitingApp { _userList      :: UserList
               , _userEnter     :: UserEnter
               , _waitingStatus :: WaitingStatus
               , _newGameState  :: GameState
               }

makeLenses ''WaitingApp

data InProgressStatus = Menu
                      | Pass
                      | Exchange
                      | Place    Position [TilePlacement]
                      deriving (Show)

type ActionList = L.List Name InProgressStatus

data InProgressApp =
    InProgressApp { _dictionary       :: [String]
                  , _gameState        :: InProgressState
                  , _inProgressStatus :: InProgressStatus
                  , _actionList       :: ActionList
                  }

makeLenses ''InProgressApp

data AppState = Waiting    WaitingApp
              | InProgress InProgressApp
              | Over

emptyUserEnter :: UserEnter
emptyUserEnter = E.editor Name.UsernameEditor (Just 1) ""

newInProgressApp :: [String] -> InProgressState -> InProgressApp
newInProgressApp dict inProgressState =
    let newActionList = L.list Name.ActionList (fromList [Pass, Exchange, Place (7, 7) []]) 1
    in  InProgressApp dict inProgressState Menu newActionList

newAppState :: StdGen -> AppState
newAppState =
    let newUserList  = L.list Name.UsernameList empty 1
    in  Waiting . WaitingApp newUserList emptyUserEnter NotEntering . newGame
