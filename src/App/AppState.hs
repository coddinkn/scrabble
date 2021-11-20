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
, TilesList
, tilesList
, inProgressStatus
, actionList
, UserList
, UserEnter
, emptyUserEnter
, WaitingStatus(..)
, InProgressStatus(..)
, PlaceStatus(..)
) where

import App.Name (Name)
import qualified App.Name as Name

import Scrabble.GameState ( Username
                          , GameState
                          , InProgressState
                          , newGame
                          , players
                          , whosTurn
                          )
import Scrabble.Position (Position)
import Scrabble.Player (playerTiles)
import Scrabble.Tile (Tile)
import Scrabble.TilePlacement (TilePlacement)

import Control.Monad.Random (StdGen)
import Data.Map ((!))
import Data.Vector (empty, fromList)

import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.Edit as E

import Lens.Micro.TH
import Lens.Micro ((^.))

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

data PlaceStatus = SelectingPosition
                 | SelectingTile
                 deriving (Eq)

data InProgressStatus = Menu
                      | Pass
                      | Exchange
                      | Place    Position [TilePlacement] PlaceStatus
                      deriving (Eq)

instance Show InProgressStatus where
    show Menu = "Menu"
    show Pass = "Pass"
    show Exchange = "Exchange"
    show (Place _ _ _) = "Place"

type ActionList = L.List Name InProgressStatus

type TilesList = L.List Name Tile

data InProgressApp =
    InProgressApp { _dictionary       :: [String]
                  , _gameState        :: InProgressState
                  , _inProgressStatus :: InProgressStatus
                  , _actionList       :: ActionList
                  , _tilesList        :: TilesList
                  }

makeLenses ''InProgressApp

data AppState = Waiting    WaitingApp
              | InProgress InProgressApp
              | Over

emptyUserEnter :: UserEnter
emptyUserEnter = E.editor Name.UsernameEditor (Just 1) ""

currentTurnTiles :: InProgressState -> [Tile]
currentTurnTiles inProgressState =
    let user = whosTurn inProgressState
        player = (inProgressState ^. players) ! user
    in player ^. playerTiles

newTilesList :: InProgressState -> TilesList
newTilesList inProgressState =
    L.list Name.TilesList (fromList $ currentTurnTiles inProgressState) 3

newInProgressApp :: [String] -> InProgressState -> InProgressApp
newInProgressApp dict inProgressState =
    let newActionList = L.list Name.ActionList (fromList [Pass, Exchange, Place (7, 7) [] SelectingPosition]) 1
    in  InProgressApp dict inProgressState Menu newActionList $ newTilesList inProgressState

newAppState :: StdGen -> AppState
newAppState =
    let newUserList  = L.list Name.UsernameList empty 1
    in  Waiting . WaitingApp newUserList emptyUserEnter NotEntering . newGame
