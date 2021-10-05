module App.AppState
( AppState(..)
, WaitingApp(..)
, newAppState
, userList
, userEnter
, enterState
, gameState
, UserList
, UserEnter
, emptyUserEnter
, EnterState(..)
) where

import App.Name

import Scrabble.GameState ( Username
                          , GameState
                          , InProgressState
                          , newGame
                          )

import Control.Monad.Random (StdGen)
import Data.Vector (empty)

import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.Edit as E

import Lens.Micro.TH

type UserList = L.List Name Username

type UserEnter = E.Editor Username Name

data EnterState = Entering | NotEntering | InvalidEntry

data WaitingApp =
    WaitingApp { _userList   :: UserList
               , _userEnter  :: UserEnter
               , _enterState :: EnterState
               , _gameState  :: GameState
               }

makeLenses ''WaitingApp

data AppState = Waiting WaitingApp
              | InProgress InProgressState
              | Over

emptyUserEnter :: UserEnter
emptyUserEnter = E.editor UsernameEditor (Just 1) ""

newAppState :: StdGen -> AppState
newAppState = let list  = L.list UsernameList empty 1
              in Waiting . WaitingApp list emptyUserEnter NotEntering . newGame
