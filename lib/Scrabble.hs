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

import System.Random (getStdGen)

type ScrabbleIO a = ScrabbleT IO a

playScrabbleIO :: ScrabbleIO () -> [String] -> IO ()
playScrabbleIO game =
    \dictionary -> do
        generator <- getStdGen
        playScrabbleT generator game putStrLn dictionary
