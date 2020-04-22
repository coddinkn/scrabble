module Scrabble
( module Scrabble
, module ScrabbleT
, module Player
, module Tile
, module TilePlacement
, module Board
) where

import Board
import Player
import Tile
import TilePlacement
import ScrabbleT

type ScrabbleIO a = ScrabbleT IO a

playScrabbleIO :: ScrabbleIO () -> [String] -> IO ()
playScrabbleIO game = playScrabbleT game putStrLn
