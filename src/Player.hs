module Player where

import Tile

import Data.Maybe (isNothing, fromMaybe)

type Username = String

data Player = Player { username  :: Username
                     , tiles     :: Tiles
                     , score     :: Int
                     } deriving Eq

type Players = [Player]

changePlayerUsername :: Username -> Player -> Player
changePlayerUsername newUsername player = player { username = newUsername }

givePlayerTiles :: Tiles -> Player -> Player
givePlayerTiles newTiles player = player { tiles = newTiles ++ oldTiles}
    where oldTiles = tiles player
