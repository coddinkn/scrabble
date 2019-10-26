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
