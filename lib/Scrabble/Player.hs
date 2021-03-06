module Scrabble.Player where

import Scrabble.Tile

import Data.List (sort, isSubsequenceOf)

data Player = Player { playerTiles    :: [Tile]
                     , playerScore    :: Int
                     , passedLastTurn :: Bool
                     }

newPlayer :: Player
newPlayer = Player [] 0 False

changePlayerScore :: (Int -> Int) -> Player -> Player
changePlayerScore modify player = player { playerScore = modify oldScore }
    where oldScore = playerScore player

givePlayerTiles :: [Tile] -> Player -> Player
givePlayerTiles newTiles player = player { playerTiles = newTiles ++ oldTiles}
    where oldTiles = playerTiles player

markPass :: Player -> Player
markPass player = player { passedLastTurn = True }

inPlayerTiles :: [Tile] -> Player -> Bool
inPlayerTiles otherTiles player =
    let playerTilesSorted = sort $ playerTiles player
        otherTilesSorted = sort otherTiles
    in otherTilesSorted `isSubsequenceOf` playerTilesSorted
