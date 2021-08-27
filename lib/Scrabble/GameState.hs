module Scrabble.GameState
( Username
, GameState (..)
, WaitingState
, InProgressState
, OverState
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
import Scrabble.Exception

import Control.Monad.Random
import Cursor.List.NonEmpty
import Data.List
import Data.List.NonEmpty (NonEmpty(..), toList)
import Data.Maybe (fromJust)
import qualified Data.Map.Strict as Map

import Lens.Micro ((^.), (.~), (%~), (&))
import Lens.Micro.TH

type Username = String

data WaitingState = WaitingState { _users :: Map.Map Username Bool
                                 , _gen   :: StdGen
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
newGame = Waiting . WaitingState Map.empty

startGame :: WaitingState -> Username -> InProgressState
startGame state first =
    InProgressState { _players = Map.fromList . zip userList $ repeat newPlayer
                    , _board = emptyBoard
                    , _tiles = evalRand getStartTiles $ state ^. gen
                    , _turnCursor = makeNonEmptyCursor id $ first :| filter (/= first) userList
                    }
    where userList = Map.keys $ state ^. users

-- shenanigans
startGameWithTiles :: [Tile] -> WaitingState -> Username -> InProgressState
startGameWithTiles startTiles state first =
    InProgressState { _players = Map.fromList . zip userList $ repeat newPlayer
                    , _board = emptyBoard
                    , _tiles = startTiles
                    , _turnCursor = makeNonEmptyCursor id $ first :| filter (/= first) userList
                    }
    where userList = Map.keys $ state ^. users

endGame :: InProgressState -> OverState
endGame state = OverState { _winner = Nothing
                          , _scores = playerScore <$> state ^. players
                          }

allReady :: WaitingState -> Bool
allReady state = and . Map.elems $ state ^. users

-- shenanigans
readyUserWithTiles :: [Tile] -> Username -> WaitingState -> GameState
readyUserWithTiles tiles username state =
    let newState = state & users %~ Map.update (Just . const True) username
    in if allReady newState
       then InProgress $ startGameWithTiles tiles newState username
       else Waiting newState

readyUser :: Username -> WaitingState -> GameState
readyUser username state =
    let newState = state & users %~ Map.update (Just . const True) username
    in if allReady newState
       then InProgress $ startGame newState username
       else Waiting newState

addUser :: Username -> WaitingState -> WaitingState
addUser username state =
    if Map.member username $ state ^. users
    then state
    else state & users %~ Map.insert username False

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
        Waiting    state -> Map.member username $ state ^. users
        InProgress state -> Map.member username $ state ^. players
        Over       state -> Map.member username $ state ^. scores

getBoard :: InProgressState -> Board
getBoard state = state ^. board

modifyBoard :: (Board -> Board) -> InProgressState -> InProgressState
modifyBoard modify state = state & board %~ modify

stepCursor :: NonEmptyCursor a a -> NonEmptyCursor a a
stepCursor cursor = maybe (nonEmptyCursorSelectFirst id id cursor) id $ nonEmptyCursorSelectNext id id cursor

nextTurn :: InProgressState -> InProgressState
nextTurn state = state & turnCursor %~ stepCursor

whosTurn :: InProgressState -> Username
whosTurn state = nonEmptyCursorCurrent $ state ^. turnCursor

getFromPlayer :: Username -> (Player -> a) -> InProgressState -> Either Exception a
getFromPlayer username get state =
    let maybePlayer = Map.lookup username $ state ^. players
    in case maybePlayer of
        Just p  -> return $ get p
        Nothing -> Left $ UnknownUser username


modifyPlayer :: Username -> (Player -> Player) -> InProgressState -> InProgressState
modifyPlayer username modify state = state & players %~ Map.adjust modify username

changeUsername :: Username -> Username -> GameState -> Either Exception GameState
changeUsername oldUsername newUsername gameState =
    case gameState of
        Waiting state ->
            case Map.lookup oldUsername $ state ^. users of
                Just ready -> return . Waiting $
                    state & users %~ Map.delete oldUsername
                          & users %~ Map.insert newUsername ready
                Nothing -> Left $ UnknownUser oldUsername
        InProgress state ->
            case Map.lookup oldUsername $ state ^. players of
                Just player -> return . InProgress $
                    state & players %~ Map.delete oldUsername
                          & players %~ Map.insert newUsername player
                          & turnCursor %~ mapNonEmptyCursor updateUser updateUser
                Nothing -> Left $ UnknownUser oldUsername
        Over state ->
            case Map.lookup oldUsername $ state ^. scores of
                Just score -> return . Over $
                    state & scores %~ Map.delete oldUsername
                          & scores %~ Map.insert newUsername score
                          & winner %~ fmap updateUser
                Nothing -> Left $ UnknownUser oldUsername
    where updateUser username = if username == oldUsername then newUsername else username
