module Tile where

newtype Tile = Tile Char
    deriving (Eq, Ord)

instance Show Tile where
    show (Tile char) = [char]

type Tiles = [Tile]
