module MonadScrabble where

import Scrabble.Board (Board)
import Scrabble.GameState ( Username )
import Scrabble.Tile ( Tile )
import Scrabble.TilePlacement ( TilePlacement )

import Data.List.NonEmpty ( NonEmpty )

class (Monad m) => MonadScrabble m where
  addPlayer :: Username -> m () 
  getScore :: Username -> m Int
  changeUsername :: Username -> Username -> m ()
  getBoard :: m Board
  placeTiles :: NonEmpty TilePlacement -> m Int
  ready :: Username -> m Bool
  readyWithTiles :: Username -> [Tile] -> m Bool
  whosTurn :: m Username
  pass :: m ()
  exchange :: Maybe Tile -> m ()
  changeScore :: Username -> (Int -> Int) -> m Int
  giveTiles :: Int -> m ()
