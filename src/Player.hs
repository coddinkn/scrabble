module Player
( Player (Player)
, Players
, Username
, tiles
, username
) where

import Board

import Data.Maybe (isNothing, fromMaybe)

type Username = String

data Player = Player { username  :: Username
                     , tiles     :: Tiles
                     , score     :: Int
                     }

type Players = [Player]
