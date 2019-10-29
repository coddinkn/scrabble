module Scrabble
( module Scrabble
, module ScrabbleT
, module Player
, module Tile
, module Board
) where

import Board
import Player
import Tile
import ScrabbleT

type ScrabbleIO a = ScrabbleT IO a

playScrabbleIO :: ScrabbleIO () -> [String] -> IO ()
playScrabbleIO game words = playScrabbleT game putStrLn words
