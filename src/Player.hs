module Player
( Player (Player)
, Players
, tiles
, username
) where

import Board

import Data.Maybe (isNothing, fromMaybe)

data Player = Player { username  :: String
                     , tiles     :: Tiles
                     , score     :: Int
                     }

type Players = [Player]
