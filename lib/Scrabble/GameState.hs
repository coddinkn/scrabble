module Scrabble.GameState
( Username
, GameState (..)
, getBoard
, modifyBoard
, modifyPlayer
, newGame
, addUser
, users
, getUsers
, readyUser
, readyUserWithTiles -- shenanigans
, checkUsername
, nextTurn
, whosTurn
, changeUsername
, giveUserTiles
, getFromPlayer
, startGame
, startGameWithTiles -- shenanigan
, endGame
) where

import Scrabble.Tile
import Scrabble.Board
import Scrabble.Player

import Control.Monad.Random
import Cursor.List.NonEmpty
import Data.List
import Data.List.NonEmpty (NonEmpty(..), toList)
import Data.Maybe (fromJust)
import qualified Data.Map.Strict as Map

import Lens.Micro ((^.), (.~), (%~), (&))
import Lens.Micro.TH

type Username = String

data WaitingState = WaitingState { _users      :: [Username]
                                 , _readyUsers :: [Username]
                                 , _gen        :: StdGen
                                 }

makeLenses ''WaitingState

data InProgressState = InProgressState { _players    :: Map.Map Username Player
                                       , _board      :: Board
                                       , _tiles      :: [Tile]
                                       , _turnCursor :: NonEmptyCursor Username Username
                                       }

makeLenses ''InProgressState

data OverState = OverState { _winner :: Maybe Username
                           , _scores :: Map.Map Username Int
                           }

makeLenses ''OverState

data GameState = Waiting    WaitingState
               | InProgress InProgressState
               | Over       OverState

shuffle :: [a] -> Rand StdGen [a]
shuffle x =
    if length x < 2
    then return x
    else do
        i <- getRandomR (0, length x - 1)
        r <- shuffle $ take i x ++ drop (i + 1) x
        return $ x !! i : r

getStartTiles :: Rand StdGen [Tile]
getStartTiles = do
    let regularTiles =
            concatMap (map Tile . uncurry replicate)
                [ (9, 'A')
                , (2, 'B')
                , (2, 'C')
                , (4, 'D')
                , (12,'E')
                , (2, 'F')
                , (3, 'G')
                , (2, 'H')
                , (9, 'I')
                , (1, 'J')
                , (1, 'K')
                , (4, 'L')
                , (2, 'M')
                , (6, 'N')
                , (8, 'O')
                , (2, 'P')
                , (1, 'Q')
                , (6, 'R')
                , (4, 'S')
                , (6, 'T')
                , (4, 'U')
                , (2, 'V')
                , (2, 'W')
                , (1, 'X')
                , (2, 'Y')
                , (1, 'Z') ]
    first  <- Tile <$> getRandomR ('A', 'Z')
    second <- Tile <$> getRandomR ('A', 'Z')
    shuffle $ regularTiles ++ [first, second]

newGame :: StdGen -> GameState
newGame = Waiting . WaitingState [] []

startGame :: WaitingState -> InProgressState
startGame (WaitingState users readyUsers generator) =
    InProgressState { _players = Map.fromList . zip users $ repeat newPlayer
                    , _board = emptyBoard
                    , _tiles = evalRand getStartTiles generator
                    , _turnCursor = makeNonEmptyCursor id $ head readyUsers :| tail readyUsers
                    }

-- shenanigans
startGameWithTiles :: [Tile] -> WaitingState -> InProgressState
startGameWithTiles startTiles (WaitingState users readyUsers generator) =
    InProgressState { _players = Map.fromList . zip users $ repeat newPlayer
                    , _board = emptyBoard
                    , _tiles = startTiles
                    , _turnCursor = makeNonEmptyCursor id $ head readyUsers :| tail readyUsers
                    }

endGame :: InProgressState -> OverState
endGame state = OverState { _winner = Nothing
                          , _scores = playerScore <$> state ^. players
                          }

allReady :: WaitingState -> Bool
allReady state = sort (state ^. readyUsers) == sort (state ^. users)

-- shenanigans
readyUserWithTiles :: [Tile] -> Username -> WaitingState -> GameState
readyUserWithTiles tiles username state
    | username `elem` alreadyReady =
        let newState = state & readyUsers .~ username:alreadyReady
        in if allReady newState
           then InProgress $ startGameWithTiles tiles newState
           else Waiting newState
    | otherwise = Waiting state
    where alreadyReady = state ^. readyUsers

readyUser :: Username -> WaitingState -> GameState
readyUser username state
    | username `elem` alreadyReady =
        let newState = state & readyUsers .~ username:alreadyReady
        in if allReady newState
           then InProgress $ startGame newState
           else Waiting newState
    | otherwise = Waiting state
    where alreadyReady = state ^. readyUsers

addUser :: Username -> WaitingState -> WaitingState
addUser username state =
    if username `elem` existingUsers
    then state
    else state & users .~ username:existingUsers
    where existingUsers = state ^. users

getUsers :: InProgressState -> [Username]
getUsers state = toList . rebuildNonEmptyCursor id $ state ^. turnCursor

giveUserTiles :: Int -> Username -> InProgressState -> InProgressState
giveUserTiles n username state =
    let tilesToGive = take n $ state ^. tiles
    in state & tiles %~ drop n
             & players %~ Map.adjust (givePlayerTiles tilesToGive) username

checkUsername :: Username -> GameState -> Bool
checkUsername username gameState =
    case gameState of
        Waiting    state -> username `elem` state ^. users
        InProgress state -> Map.member username $ state ^. players
        Over       state -> Map.member username $ state ^. scores

getBoard :: InProgressState -> Board
getBoard state = state ^. board

modifyBoard :: (Board -> Board) -> GameState -> GameState
modifyBoard modify (InProgress state) = InProgress $ state & board %~ modify

stepCursor :: NonEmptyCursor a a -> NonEmptyCursor a a
stepCursor cursor = maybe (nonEmptyCursorSelectFirst id id cursor) id $ nonEmptyCursorSelectNext id id cursor

nextTurn :: InProgressState -> InProgressState
nextTurn state = state & turnCursor %~ stepCursor

whosTurn :: InProgressState -> Username
whosTurn state = nonEmptyCursorCurrent $ state ^. turnCursor

getFromPlayer :: Username -> (Player -> a) -> GameState -> Maybe a
getFromPlayer username get gameState =
    case gameState of
        InProgress state -> fmap get . Map.lookup username $ state ^. players
        _ -> Nothing

modifyPlayer :: Username -> (Player -> Player) -> GameState -> GameState
modifyPlayer username modify (InProgress state) = InProgress $ state & players %~ Map.adjust modify username

changeUsername :: Username -> Username -> GameState -> GameState
changeUsername oldUsername newUsername gameState =
    case gameState of
        Waiting state -> Waiting $
            state & users %~ map updateUser
                  & readyUsers %~ map updateUser
        InProgress state ->
            case Map.lookup oldUsername $ state ^. players of
                Just player -> InProgress $
                    state & players %~ Map.delete oldUsername
                          & players %~ Map.insert newUsername player
                          & turnCursor %~ mapNonEmptyCursor updateUser updateUser
                Nothing -> gameState
        _ -> gameState
    where updateUser username = if username == oldUsername then newUsername else username
