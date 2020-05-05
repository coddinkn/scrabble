module GameState where

import Tile
import Board
import Player hiding (tiles)

import Data.List
import Data.Maybe (fromJust)
import qualified Data.Map.Strict as Map

type Username = String

data GameState = GameState { players  :: Map.Map Username Player
                           , board    :: Board
                           , tiles    :: [Tile]
                           , order    :: [Username]
                           , whosTurn :: Maybe Username
                           }

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
newGame = GameState Map.empty emptyBoard startTiles [] Nothing

ready :: Username -> GameState -> GameState
ready username gameState =
    if username `elem` turnOrder
    then gameState
    else gameState { order = username:turnOrder }
    where turnOrder = order gameState

started :: GameState -> Bool
started gameState =
    let allPlayers   = Map.keys $ players gameState
        readyPlayers = order gameState
    in all (`elem` readyPlayers) allPlayers

addPlayer :: Username -> GameState -> GameState
addPlayer username gameState = gameState { players = Map.insert username newPlayer existingPlayers }
    where existingPlayers = players gameState

givePlayerTiles :: Username -> Int -> GameState -> GameState
givePlayerTiles username n gameState = modifyPlayer username (Player.givePlayerTiles tilesToGive) $ gameState { tiles = newTiles }
    where tilesToGive = take n $ tiles gameState
          newTiles = drop n $ tiles gameState

checkUsername :: Username -> GameState -> Bool
checkUsername username gameState = Map.member username $ players gameState

getPlayer :: Username -> GameState -> Player
getPlayer username = fromJust . Map.lookup username . players

modifyBoard :: (Board -> Board) -> GameState -> GameState
modifyBoard modify gameState = gameState { board = modify currentBoard }
    where currentBoard = board gameState

setTurn :: Username -> GameState -> GameState
setTurn username gameState = gameState { whosTurn = Just username }

nextTurn :: GameState -> GameState
nextTurn gameState =
    gameState { whosTurn = do current <- whosTurn gameState
                              let turnOrder = order gameState
                              currentIndex <- current `elemIndex` turnOrder
                              let nextIndex = (currentIndex + 1) `mod` (length turnOrder)
                              return $ turnOrder !! nextIndex
              }

getFromPlayer :: Username -> (Player -> a) -> GameState -> a
getFromPlayer username get = get . getPlayer username

modifyPlayer :: Username -> (Player -> Player) -> GameState -> GameState
modifyPlayer username modify gameState = gameState { players = Map.adjust modify username oldPlayers }
    where oldPlayers = players gameState

changeUsername :: Username -> Username -> GameState -> GameState
changeUsername oldUsername newUsername gameState =
    let playersWithout = Map.delete oldUsername $ players gameState
        maybePlayer = Map.lookup oldUsername $ players gameState
    in case maybePlayer of
        Just player -> gameState { players = Map.insert newUsername player playersWithout }
        Nothing -> gameState
