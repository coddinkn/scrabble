module GameState
( Username
, GameState (..)
, modifyBoard
, modifyPlayer
, GameStatus (..)
, newGame
, getStatus
, addUser
, readyUser
, checkUsername
, nextTurn
, changeUsername
, giveUserTiles
, getFromPlayer
, startGame
, endGame
) where

import Tile
import Board
import Player hiding (tiles)

import Data.List
import Data.Maybe (fromJust)
import qualified Data.Map.Strict as Map

type Username = String

data GameStatus = WaitingToStart
                | Started
                | Ended
                deriving Eq

data GameState = Waiting { users      :: [Username]
                         , readyUsers :: [Username]
                         }

               | InProgress { players   :: Map.Map Username Player
                            , board     :: Board
                            , tiles     :: [Tile]
                            , turnOrder :: [Username]
                            , whosTurn  :: Username
                            }

               | Over { winner :: Maybe Username
                      , scores :: Map.Map Username Int
                      }

getStatus :: GameState -> GameStatus
getStatus gameState =
    case gameState of
         Waiting    {} -> WaitingToStart
         InProgress {} -> Started
         Over       {} -> Ended

startTiles :: [Tile]
startTiles = map Tile "BATCATSBATCATS"
{-
    concat $ map (map Tile . uncurry replicate)
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
-}

newGame :: GameState
newGame = Waiting [] []

startGame :: GameState -> GameState
startGame gameState =
    case gameState of
        Waiting users readyUsers ->
            InProgress { players = Map.fromList $ zip users $ repeat newPlayer
                       , board = emptyBoard
                       , tiles = startTiles
                       , turnOrder = readyUsers
                       , whosTurn = head readyUsers
                       }
        _ -> gameState

endGame :: GameState -> GameState
endGame gameState =
    case gameState of
        InProgress {} -> Over { winner = Nothing
                              , scores = fmap playerScore $ players gameState
                              }
        _ -> gameState

readyUser :: Username -> GameState -> GameState
readyUser username gameState =
    case gameState of
        Waiting users alreadyReady ->
            if username `elem` alreadyReady
            then gameState
            else let newReadyUsers = username:alreadyReady
                     newGameState = gameState { readyUsers = newReadyUsers }
                 in if (sort newReadyUsers) == (sort users)
                    then startGame newGameState
                    else newGameState
        _ -> gameState

addUser :: Username -> GameState -> GameState
addUser username gameState =
    case gameState of
        Waiting existingUsers _ ->
            if username `elem` existingUsers
            then gameState
            else gameState { users = username:existingUsers }
        _ -> gameState

giveUserTiles :: Int -> Username -> GameState -> GameState
giveUserTiles n username gameState =
    case gameState of
        InProgress {} ->
            modifyPlayer username (Player.givePlayerTiles tilesToGive) $ gameState { tiles = newTiles }
            where tilesToGive = take n $ tiles gameState
                  newTiles = drop n $ tiles gameState
        _ -> gameState

checkUsername :: Username -> GameState -> Bool
checkUsername username gameState =
    case gameState of
        Waiting    {} -> username `elem` users gameState
        InProgress {} -> Map.member username $ players gameState
        Over       {} -> Map.member username $ scores gameState

modifyBoard :: (Board -> Board) -> GameState -> GameState
modifyBoard modify gameState = gameState { board = modify currentBoard }
    where currentBoard = board gameState

nextTurn :: GameState -> GameState
nextTurn gameState =
    case gameState of
        InProgress {} ->
            let currentIndex = fromJust $ current `elemIndex` order
                nextIndex = (currentIndex + 1) `mod` (length order)
                next = order !! nextIndex
            in gameState { whosTurn = next }
            where current = whosTurn gameState
                  order = turnOrder gameState
        _ -> gameState

getFromPlayer :: Username -> (Player -> a) -> GameState -> Maybe a
getFromPlayer username get gameState =
    case gameState of
        InProgress {} -> fmap get . Map.lookup username $ players gameState
        _ -> Nothing

modifyPlayer :: Username -> (Player -> Player) -> GameState -> GameState
modifyPlayer username modify gameState = gameState { players = Map.adjust modify username oldPlayers }
    where oldPlayers = players gameState

changeUsername :: Username -> Username -> GameState -> GameState
changeUsername oldUsername newUsername gameState =
    case gameState of
        Waiting {} ->
            let usersWithout = delete oldUsername $ users gameState
                readyWithout = delete oldUsername $ readyUsers gameState
            in if readyWithout == readyUsers gameState
               then gameState { users = newUsername:usersWithout }
               else gameState { users = newUsername:usersWithout, readyUsers = newUsername:readyWithout }
        InProgress {} ->
            let playersWithout = Map.delete oldUsername $ players gameState
                maybePlayer = Map.lookup oldUsername $ players gameState
            in case maybePlayer of
                Just player -> gameState { players = Map.insert newUsername player playersWithout }
                Nothing -> gameState
        _ -> gameState
