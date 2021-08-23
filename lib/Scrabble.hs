module Scrabble
( module ScrabbleT
, Scrabble
, playScrabble
, ScrabbleIO
, playScrabbleIO
) where

import Scrabble.GameState
import ScrabbleT

import Control.Monad.Identity

type Scrabble a = ScrabbleT Identity a

playScrabble :: [String] -> GameState -> Scrabble () -> Either String GameState
playScrabble dictionary gameState scrabble =
    runIdentity $ playScrabbleT dictionary gameState scrabble

type ScrabbleIO a = ScrabbleT IO a

playScrabbleIO :: [String] -> GameState -> ScrabbleIO () -> IO (Either String GameState)
playScrabbleIO = playScrabbleT
