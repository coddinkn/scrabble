module Scrabble
( module MonadScrabble
, Scrabble
, playScrabble
, ScrabbleIO
, playScrabbleIO
) where

import Scrabble.GameState
import Scrabble.Exception
import ScrabbleT
import MonadScrabble

import Control.Monad.Identity

newtype Scrabble a = Scrabble {
    runScrabble :: ScrabbleT Identity a
} deriving (Functor, Applicative, Monad, MonadScrabble)

playScrabble :: [String] -> GameState -> Scrabble () -> Either Exception GameState
playScrabble dictionary gameState scrabble =
    runIdentity . playScrabbleT dictionary gameState $ runScrabble scrabble

newtype ScrabbleIO a = ScrabbleIO {
    runScrabbleIO :: ScrabbleT IO a
} deriving (Functor, Applicative, Monad, MonadScrabble)

playScrabbleIO :: [String] -> GameState -> ScrabbleIO () -> IO (Either Exception GameState)
playScrabbleIO dictionary gameState = playScrabbleT dictionary gameState . runScrabbleIO
