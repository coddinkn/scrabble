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
, newTilesList
, newInProgressApp
, ActionList
, TilesList
, tilesList
, tilePlacements
, inProgressStatus
, actionList
, UserList
, UserEnter
, emptyUserEnter
, WaitingStatus(..)
, InProgressStatus(..)
, PlaceStatus(..)
, evalScrabbleApp
, playScrabbleApp
) where

import App.Name (Name)
import qualified App.Name as Name

import Scrabble ( Scrabble
                , playScrabble
                , evalScrabble
                )
import Scrabble.GameState ( Username
                          , GameState
                          , InProgressState
                          , newGame
                          , players
                          , whosTurn
                          )
import qualified Scrabble.GameState as GS
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
import Lens.Micro ((^.), (.~), (&))

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
                      | Place      Position PlaceStatus
                      | Play
                      | PlaceError String
                      deriving (Eq)

instance Show InProgressStatus where
    show Menu = "Menu"
    show Pass = "Pass"
    show Exchange = "Exchange"
    show (Place _ _) = "Place"
    show Play = "Play word"
    show (PlaceError str) = str

type ActionList = L.List Name InProgressStatus

type TilesList = L.List Name Tile

data InProgressApp =
    InProgressApp { _dictionary       :: [String]
                  , _gameState        :: InProgressState
                  , _inProgressStatus :: InProgressStatus
                  , _tilePlacements   :: [TilePlacement]
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
    let newActionList = L.list Name.ActionList (fromList [Pass, Exchange, Place (7, 7) SelectingPosition, Play]) 1
    in  InProgressApp dict inProgressState Menu [] newActionList $ newTilesList inProgressState

newAppState :: StdGen -> AppState
newAppState =
    let newUserList  = L.list Name.UsernameList empty 1
    in  Waiting . WaitingApp newUserList emptyUserEnter NotEntering . newGame

playScrabbleApp :: InProgressApp -> Scrabble () -> AppState
playScrabbleApp app scrabble =

    case playScrabble (app ^. dictionary) (GS.InProgress $ app ^. gameState) scrabble of

        Right (GS.Waiting _) -> undefined -- A transition back to waiting should never occur

        Right (GS.InProgress inProgressGameState) -> InProgress $ app & gameState .~ inProgressGameState

        Right (GS.Over _) -> Over

        Left exception -> error $ show exception

evalScrabbleApp :: InProgressApp -> Scrabble a -> a
evalScrabbleApp app scrabble =

    case evalScrabble (app ^. dictionary) (GS.InProgress $ app ^. gameState) scrabble of

        Right result -> result

        Left exception -> error $ show exception
