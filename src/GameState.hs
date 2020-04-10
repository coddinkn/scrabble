module GameState where

import Tile
import Board
import Player hiding (tiles)

import Data.List

data GameState = GameState { players  :: Players
                           , board    :: Board
                           , tiles    :: [Tile]
                           , whosTurn :: Maybe Player
                           }

startTiles :: [Tile]
startTiles = map Tile "BATCATSBATCATS"

newGame :: GameState
newGame = GameState [] emptyBoard startTiles Nothing

addPlayer :: Username -> GameState -> GameState
addPlayer username gameState = let newPlayer = Player username [] 0
                                   existingPlayers = players gameState
                               in gameState { players = newPlayer : existingPlayers }

givePlayerTiles :: Player -> Int -> GameState -> GameState
givePlayerTiles player n gameState = modifyPlayer player (Player.givePlayerTiles tilesToGive) $ gameState { tiles = newTiles }
    where tilesToGive = take n $ tiles gameState
          newTiles = drop n $ tiles gameState

getPlayer :: Username -> GameState -> Maybe Player
getPlayer playerUsername gameState = find sameUsername $ players gameState
    where sameUsername player = playerUsername == username player

modifyBoard :: (Board -> Board) -> GameState -> GameState
modifyBoard modify gameState = gameState { board = modify currentBoard }
    where currentBoard = board gameState

modifyPlayer :: Player -> (Player -> Player) -> GameState -> GameState
modifyPlayer player modify gameState =
    let newPlayer = modify player
        otherPlayers = filter (/= player) $ players gameState
    in gameState { players = newPlayer : otherPlayers }
