module Scrabble.Exception
( Exception (..)
) where

data Exception = IncorrectState        String
               | UsernameTaken         String
               | UnknownUser           String
               | InvalidWord           String
               | ImproperTilePlacement String
               | IncorrectTiles        String String

instance Show Exception where

    show (IncorrectState message) = "Incorrect game state for operation: " ++ message

    show (UsernameTaken username) = "A user with username " ++ username ++ " already exists"

    show (UnknownUser username) = "No user with username " ++ username

    show (InvalidWord word) = word ++ " is not in the provided dictionary"

    show (ImproperTilePlacement message) = "Improper tile placement: " ++ message

    show (IncorrectTiles playedTiles playerTiles) = "Incorrect tiles: " ++ playedTiles ++ " not a subset of " ++ playerTiles
