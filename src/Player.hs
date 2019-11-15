module Player where

import Tile

import Data.Maybe (isNothing, fromMaybe)
import Data.List (sort)

type Username = String

data Player = Player { username  :: Username
                     , tiles     :: [Tile]
                     , score     :: Int
                     } deriving Eq

type Players = [Player]

changePlayerUsername :: Username -> Player -> Player
changePlayerUsername newUsername player = player { username = newUsername }

changePlayerScore :: (Int -> Int) -> Player -> Player
changePlayerScore modify player = player { score = modify oldScore }
    where oldScore = score player

givePlayerTiles :: [Tile] -> Player -> Player
givePlayerTiles newTiles player = player { tiles = sort $ newTiles ++ oldTiles}
    where oldTiles = tiles player
