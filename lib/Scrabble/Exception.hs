module Scrabble.Exception
( Exception (..)
) where

data Exception = IncorrectState String
               | UsernameTaken String
               | UnknownUser String
               | Generic String

instance Show Exception where

    show (IncorrectState message) = "Incorrect game state for operation: " ++ message

    show (UsernameTaken username) = "A user with username " ++ username ++ " already exists"

    show (UnknownUser username) = "No user with username " ++ username

    show (Generic message) = message
