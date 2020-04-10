module Board
( Position
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

import Modifier
import Position
import Tile
import TilePlacement

import Prelude hiding (lookup)
import Data.Map hiding (map)
import Data.List (find)
import Data.Maybe

boardMax = 14
boardMin = 0

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

getTiles :: Board -> TilePlacement -> Orientation -> Direction -> [Tile]
getTiles board tilePlacement orientation direction = adjust . getConnectedTiles . adjust $ map makePosition range
    where (column, row) = position tilePlacement
          (limit, makePosition) = case orientation of
                                       Vertical -> (column, \n -> (n, row))
                                       Horizontal -> (row, \n -> (column, n))
          (range, adjust) = case direction of
                       Before -> ([boardMin .. (limit - 1)], reverse)
                       After -> ([(limit + 1) .. boardMax], id)
          getConnectedTiles = map fromJust . takeWhile isJust . map (getTile board)

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

getPlacedTiles :: Board -> TilePlacement -> Orientation -> Direction -> [TilePlacement]
getPlacedTiles board tilePlacement orientation direction = adjust . connectedTilePlacements . adjust $ map makePosition range
    where (column, row) = position tilePlacement
          (limit, makePosition) = case orientation of
                                       Vertical -> (column, \n -> (n, row))
                                       Horizontal -> (row, \n -> (column, n))
          (range, adjust) = case direction of
                       Before -> ([boardMin .. (limit - 1)], reverse)
                       After  -> ([(limit + 1) .. boardMax], id)
          connectedTilePlacements positions = let tiles = map fromJust . takeWhile isJust $ map (getTile board) positions
                                              in zipWith TilePlacement tiles positions

getWordFromTilePlacement :: Board -> TilePlacement -> Orientation -> String
getWordFromTilePlacement board tilePlacement orientation = before ++ show (tile tilePlacement) ++ after
    where before = getTiles board tilePlacement orientation Before
          after  = getTiles board tilePlacement orientation After

getWordFromTilePlacements :: Board -> [TilePlacement] -> Maybe String
getWordFromTilePlacements board tilePlacements = do orientation <- orientation $ position <$> tilePlacements
                                                    let before = getTiles board (head tilePlacements) orientation Before
                                                        after  = getString board (last tilePlacements) orientation After
                                                    between <- getStringBetween board tilePlacements orientation
                                                    return $ mconcat [before, between, after]
