module Scrabble
( module MonadScrabble
, Scrabble
, playScrabble
, evalScrabble
, ScrabbleIO
, playScrabbleIO
, evalScrabbleIO
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

evalScrabble :: [String] -> GameState -> Scrabble a -> Either Exception a
evalScrabble dictionary gameState scrabble =
    runIdentity . evalScrabbleT dictionary gameState $ runScrabble scrabble

newtype ScrabbleIO a = ScrabbleIO {
    runScrabbleIO :: ScrabbleT IO a
} deriving (Functor, Applicative, Monad, MonadScrabble)

playScrabbleIO :: [String] -> GameState -> ScrabbleIO () -> IO (Either Exception GameState)
playScrabbleIO dictionary gameState = playScrabbleT dictionary gameState . runScrabbleIO

evalScrabbleIO :: [String] -> GameState -> ScrabbleIO a -> IO (Either Exception a)
evalScrabbleIO dictionary gameState = evalScrabbleT dictionary gameState . runScrabbleIO
