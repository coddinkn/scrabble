module Tile where

newtype Tile = Tile Char
    deriving (Eq, Ord, Show)

type Tiles = [Tile]
