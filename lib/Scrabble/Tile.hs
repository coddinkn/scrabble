module Scrabble.Tile
( Tile (..)
) where

import Scrabble.Score

newtype Tile = Tile Char
    deriving (Eq, Ord)

instance Show Tile where
    show (Tile char) = pure char

tileScore :: Tile -> Int
tileScore (Tile letter)
    | letter `elem` "AEILNORSTU" = 1
    | letter `elem` "DG"         = 2
    | letter `elem` "BCMP"       = 3
    | letter `elem` "FHVWY"      = 4
    | letter == 'K'              = 5
    | letter `elem` "JX"         = 8
    | letter `elem` "QZ"         = 10
    | otherwise                  = 0

instance Scorable Tile where
    score tile = Score 1 $ tileScore tile
