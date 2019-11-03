module GameState where

import Tile
import Board
import Player hiding (tiles)

import Data.List

data GameState = GameState { players  :: Players
                           , board    :: Board
                           , tiles    :: Tiles
                           , whosTurn :: Maybe Player
                           }

startTiles :: Tiles
startTiles = replicate 100 $ Tile 'A'

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

modifyPlayer :: Player -> (Player -> Player) -> GameState -> GameState
modifyPlayer player modify gameState =
    let newPlayer = modify player
        otherPlayers = filter (/= player) $ players gameState
    in gameState { players = newPlayer : otherPlayers }
