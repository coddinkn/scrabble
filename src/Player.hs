module Player where

import Tile

import Data.Maybe (isNothing, fromMaybe)
import Data.List (sort)

type Username = String

data Player = Player { username    :: Username
                     , tiles       :: [Tile]
                     , playerScore :: Int
                     } deriving Eq

changePlayerUsername :: Username -> Player -> Player
changePlayerUsername newUsername player = player { username = newUsername }

changePlayerScore :: (Int -> Int) -> Player -> Player
changePlayerScore modify player = player { playerScore = modify oldScore }
    where oldScore = playerScore player

givePlayerTiles :: [Tile] -> Player -> Player
givePlayerTiles newTiles player = player { tiles = sort $ newTiles ++ oldTiles}
    where oldTiles = tiles player
