module Board
( Board
, Tile (Tile)
, Tiles
, Modifier
, emptyBoard
) where

import Data.Map
import Data.Maybe

newtype Tile = Tile Char
    deriving (Eq, Ord, Show)

type Tiles = [Tile]

data BoardTile = BoardTile (Maybe Tile) (Maybe Modifier)
    deriving Eq

instance Show BoardTile where
    show (BoardTile maybeTile maybeModifier) = maybe emptyTile show maybeTile
        where emptyTile = maybe " " show maybeModifier 

data Modifier = DoubleLetter
              | TripleLetter
              | DoubleWord
              | TripleWord
              deriving (Eq)

instance Show Modifier where
    show DoubleLetter = "1"
    show TripleLetter = "2"
    show DoubleWord   = "3"
    show TripleWord   = "4"

newtype Board = Board (Map (Int, Int) BoardTile)
    deriving Eq

instance Show Board where
    show (Board boardMap)= concat $ showSpace <$> toAscList boardMap
         where showSpace ((x, y), boardTile) = show boardTile ++ 
                                                    if y == 14 && x /= 14
                                                    then "\n"
                                                    else ""

spaceModifier :: (Int, Int) -> Maybe Modifier
spaceModifier space
    | space `elem` [ (0,  3), (0,  11)
                   , (2,  6), (2,   8)
                   , (3,  0), (3,   7), (3,  14)
                   , (6,  2), (6,   6), (6,   8), (6, 12)
                   , (7,  3), (7,  11)
                   , (8,  2), (8,   6), (8,   8), (8, 12)
                   , (11, 0), (11,  7), (11, 14)
                   , (12, 6), (12,  8)
                   , (14, 3), (14, 11)
                   ] = Just DoubleLetter
    | space `elem` [ (1,  5), (1,  9)
                   , (5,  1), (5,  5), (5, 9), (5, 13)
                   , (9,  1), (9,  5), (9, 9), (9, 13)
                   , (13, 5), (13, 9)
                   ] = Just TripleLetter 
    | space `elem` [ (1,   1), (2,   2), (3,   3), (4,   4)
                   , (13,  1), (12,  2), (11,  3), (10,  4)
                   , (1,  13), (2,  12), (3,  11), (4,  10)
                   , (13, 13), (12, 12), (11, 11), (10, 10)
                   ] = Just DoubleWord 
    | space `elem` [ (0,  0), (0,  7), (0,  14)
                   , (7,  0), (7, 14)
                   , (14, 0), (14, 7), (14, 14) 
                   ] = Just TripleWord 
    | otherwise = Nothing

makeBoardTile :: (Int, Int) -> BoardTile
makeBoardTile space = BoardTile Nothing $ spaceModifier space

emptyBoard :: Board
emptyBoard = Board . fromDistinctAscList $ makeBoardSpace <$> spaces
    where spaces = (,) <$> [0..14] <*> [0..14]
          makeBoardSpace space = (space, makeBoardTile space)
