module Scrabble
( module ScrabbleT
, ScrabbleIO
, playScrabbleIO
) where

import Scrabble.Board
import Scrabble.Player
import Scrabble.Tile
import Scrabble.TilePlacement
import ScrabbleT

import System.Random (getStdGen)

type ScrabbleIO a = ScrabbleT IO a

playScrabbleIO :: ScrabbleIO () -> [String] -> IO ()
playScrabbleIO game =
    \dictionary -> do
        generator <- getStdGen
        playScrabbleT generator game putStrLn dictionary
