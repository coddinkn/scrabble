module Scrabble.Position
( Position
, Orientation (..)
, orientation
, opposite
) where

type Position = (Int, Int)

data Orientation = Vertical
                 | Horizontal
                 deriving (Eq, Show)

opposite :: Orientation -> Orientation
opposite Vertical = Horizontal
opposite Horizontal = Vertical

inLine :: [Position] -> Orientation -> Bool
inLine spaces orientation = all (== c) cs
    where choose = case orientation of
                     Vertical   -> snd
                     Horizontal -> fst
          cs = map choose spaces
          c = head cs

orientation :: [Position] -> Maybe Orientation
orientation spaces
    | inLine spaces Horizontal = Just Horizontal
    | inLine spaces Vertical = Just Vertical
    | otherwise = Nothing
