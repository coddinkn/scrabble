module Scrabble.Exception
( Exception (..)
) where

data Exception = IncorrectState String
               | Generic String

instance Show Exception where

    show (IncorrectState message) = "Incorrect game state for operation: " ++ message

    show (Generic message) = message
