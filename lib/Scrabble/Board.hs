module Scrabble.Board
( Position
, Board
, emptyBoard
, getTile
, putTile
, showTile
, boardMin
, boardMax
, middle
) where

import Scrabble.Modifier
import Scrabble.Position
import Scrabble.Tile
import Scrabble.TilePlacement

import Prelude hiding (lookup)
import Data.Map hiding (map)

boardMax = 14
boardMin = 0

middle :: Position
middle = (7, 7)

newtype Board = Board (Map Position Tile)
    deriving Eq

tiles :: Board -> Map Position Tile
tiles (Board tileMap) = tileMap

showTile :: Board -> Position -> String
showTile board position =
    let maybeTile = getTile board position
        underTile = maybe (if position == middle then "*" else " ") show (modifier position)
    in maybe underTile show maybeTile

showPosition :: Board -> Position -> String
showPosition board position =
    let maybeTile = getTile board position
        noTile = maybe " " show $ modifier position
        (column, row) = position
    in maybe noTile show maybeTile
        ++ if row == boardMax && column /= boardMax
           then "\n"
           else ""

instance Show Board where
    show board = positions >>= showPosition board
        where positions = (,) <$> [boardMin .. boardMax] <*> [boardMin .. boardMax]

emptyBoard :: Board
emptyBoard = Board empty

getTile :: Board -> Position -> Maybe Tile
getTile board position = lookup position $ tiles board

putTile :: TilePlacement -> Board -> Board
putTile (TilePlacement tile position) board = Board $ insert position tile $ tiles board
