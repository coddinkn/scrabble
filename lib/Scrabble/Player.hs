module Scrabble.Player where

import Scrabble.Tile

import Data.List (sort, isSubsequenceOf)
import Data.List.NonEmpty (NonEmpty, toList)

import Lens.Micro ((^.), (.~), (%~), (&))
import Lens.Micro.TH

data Player = Player { _playerTiles    :: [Tile]
                     , _playerScore    :: Int
                     , _passedLastTurn :: Bool
                     }

makeLenses ''Player

newPlayer :: Player
newPlayer = Player [] 0 False

inPlayerTiles :: NonEmpty Tile -> Player -> Bool
inPlayerTiles otherTiles player =
    let playerTilesSorted = sort $ player ^. playerTiles
        otherTilesSorted = sort $ toList otherTiles
    in otherTilesSorted `isSubsequenceOf` playerTilesSorted
