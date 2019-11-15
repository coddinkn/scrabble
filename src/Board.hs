module Board
( TilePlacement (TilePlacement)
, Position
, Board
, putTile
, tile
, position
, getWordFromTilePlacement
, getWordFromTilePlacements
, orientation
, opposite
, emptyBoard
) where

import Tile

import Prelude hiding (lookup)
import Data.Map hiding (map)
import Data.List (find)
import Data.Maybe

boardMax = 14
boardMin = 0

type Position = (Int, Int)

data Modifier = DoubleLetter
              | TripleLetter
              | DoubleWord
              | TripleWord
              deriving Eq

instance Show Modifier where
    show DoubleLetter = "1"
    show TripleLetter = "2"
    show DoubleWord   = "3"
    show TripleWord   = "4"

data TilePlacement = TilePlacement { tile :: Tile
                                   , position :: Position
                                   } deriving Eq

newtype Board = Board (Map Position Tile)
    deriving Eq

tiles :: Board -> Map Position Tile
tiles (Board tileMap) = tileMap

showPosition :: Board -> Position -> String
showPosition board position = let maybeTile = getTile board position
                                  noTile = maybe " " show $ modifier position
                                  (column, row) = position
                              in maybe noTile show maybeTile ++ if row == boardMax && column /= boardMax
                                                                then "\n"
                                                                else ""

instance Show Board where
    show board = positions >>= showPosition board
        where positions = (,) <$> [boardMin .. boardMax] <*> [boardMin .. boardMax]

modifier :: Position -> Maybe Modifier
modifier position
    | position `elem` [ (0,  3), (0,  11)
                   , (2,  6), (2,   8)
                   , (3,  0), (3,   7), (3,  14)
                   , (6,  2), (6,   6), (6,   8), (6, 12)
                   , (7,  3), (7,  11)
                   , (8,  2), (8,   6), (8,   8), (8, 12)
                   , (11, 0), (11,  7), (11, 14)
                   , (12, 6), (12,  8)
                   , (14, 3), (14, 11)
                   ] = Just DoubleLetter
    | position `elem` [ (1,  5), (1,  9)
                   , (5,  1), (5,  5), (5, 9), (5, 13)
                   , (9,  1), (9,  5), (9, 9), (9, 13)
                   , (13, 5), (13, 9)
                   ] = Just TripleLetter 
    | position `elem` [ (1,   1), (2,   2), (3,   3), (4,   4)
                   , (13,  1), (12,  2), (11,  3), (10,  4)
                   , (1,  13), (2,  12), (3,  11), (4,  10)
                   , (13, 13), (12, 12), (11, 11), (10, 10)
                   ] = Just DoubleWord 
    | position `elem` [ (0,  0), (0,  7), (0,  14)
                   , (7,  0), (7, 14)
                   , (14, 0), (14, 7), (14, 14) 
                   ] = Just TripleWord 
    | otherwise = Nothing

emptyBoard :: Board
emptyBoard = Board empty

getTile :: Board -> Position -> Maybe Tile
getTile board position = lookup position $ tiles board

putTile :: TilePlacement -> Board -> Board
putTile (TilePlacement tile position) board = Board $ insert position tile $ tiles board

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
    | inLine spaces Vertical = Just Vertical
    | inLine spaces Horizontal = Just Horizontal
    | otherwise = Nothing


getCharBetween :: Board -> [TilePlacement] -> Position -> Maybe Char
getCharBetween board tilePlacements posn = maybe (head . show <$> getTile board posn)
                                                 return
                                                 $ head . show . tile <$> tilePlacement
    where tilePlacement = find (\tilePlacement -> position tilePlacement == posn) tilePlacements

getStringBetween :: Board -> [TilePlacement] -> Orientation -> Maybe String
getStringBetween board tilePlacements orientation = mapM (getCharBetween board tilePlacements) range
    where positions = map position tilePlacements
          (constant, selector) = case orientation of
                                      Vertical -> (snd $ head positions, fst)
                                      Horizontal -> (fst $ head positions, snd)
          max = maximum $ map selector positions
          min = minimum $ map selector positions
          range = case orientation of
                       Vertical -> [min .. max] `zip` repeat constant
                       Horizontal -> repeat constant `zip` [min .. max]

data Direction = Before
               | After

getString :: Board -> TilePlacement -> Orientation -> Direction -> String
getString board tilePlacement orientation direction = (adjust . getConnectedTiles . adjust $ map makePosition range) >>= show
    where (column, row) = position tilePlacement
          (limit, makePosition) = case orientation of
                                       Vertical -> (column, \n -> (n, row))
                                       Horizontal -> (row, \n -> (column, n))
          (range, adjust) = case direction of
                       Before -> ([boardMin .. (limit - 1)], reverse)
                       After -> ([(limit + 1) .. boardMax], id)
          getConnectedTiles = map fromJust . takeWhile isJust . map (getTile board)

getWordFromTilePlacement :: Board -> TilePlacement -> Orientation -> String
getWordFromTilePlacement board tilePlacement orientation = before ++ show (tile tilePlacement) ++ after
    where before = getString board tilePlacement orientation Before
          after = getString board tilePlacement orientation After

getWordFromTilePlacements :: Board -> [TilePlacement] -> Maybe String
getWordFromTilePlacements board tilePlacements = do orientation <- orientation $ position <$> tilePlacements
                                                    let before = getString board (head tilePlacements) orientation Before
                                                        after = getString board (last tilePlacements) orientation After
                                                    between <- getStringBetween board tilePlacements orientation
                                                    return $ mconcat [before, between, after]
