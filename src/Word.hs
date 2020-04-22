module Word 
( Word
, Dictionary
, getWords
) where

import Board
import Position
import Tile
import TilePlacement
import Score

import Data.List (sortBy)
import Data.Maybe (fromJust, isJust, catMaybes)
import Prelude hiding (Word)

newtype Letter = Letter (Either Tile TilePlacement)

instance Scorable Letter where
    score (Letter value) = either score score value

instance Show Letter where
    show (Letter value) = either show show value

tileToLetter :: Tile -> Letter
tileToLetter = Letter . Left

tilePlacementToLetter :: TilePlacement -> Letter
tilePlacementToLetter = Letter . Right

newtype Word = Word [Letter]

instance Scorable Word where
    score (Word letters) = mconcat $ map score letters

instance Show Word where
    show (Word letters) = letters >>= show

data Direction = Before
               | After

type Dictionary = [String]

checkWord :: Dictionary -> Word -> Either String Word
checkWord dictionary word =
    if show word `elem` dictionary
    then Right word
    else Left $ show word ++ " is not in the dictionary"

getWords :: Dictionary -> Board -> [TilePlacement] -> Either String [Word]
getWords dictionary board tilePlacements =
    case orientation $ position <$> tilePlacements of
      Just orientation ->
        let words = catMaybes $ map getPerpWord tilePlacements
        in mapM (checkWord dictionary) words
        where getPerpWord = getWordFromTilePlacement board $ opposite orientation
      Nothing -> Left "Tiles not placed in single row or column"
{-
getWord :: Dictionary -> Board -> Orientation -> [TilePlacement] -> Either String Word
getWord dictionary board orientation tilePlacements =
    let before  = getLetters board (head sorted) orientation Before
        after   = getLetters board (last sorted) orientation After
        between = getLettersBetween board sorted orientation
        word = Word $ before ++ between ++ after
    in if show word `elem` dictionary
       then Right word
       else Left $ show word ++ " is not in the dictionary"
    where sorted = sortBy (lineUp orientation) tilePlacements

getLettersBetween :: Board -> [TilePlacement] -> Position -> Letter
getLettersBetween board tilePlacements posn = maybe (getTile board posn)
                                                   return
                                                   $ head . show . tile <$> tilePlacement
    where tilePlacement = find (\tilePlacement -> position tilePlacement == posn) tilePlacements
-}
getLetters :: Board -> TilePlacement -> Orientation -> Direction -> [Letter]
getLetters board tilePlacement orientation direction = 
    let (column, row) = position tilePlacement
        (limit, makePosition) =
            case orientation of
                Vertical   -> (column, \n -> (n, row))
                Horizontal -> (row, \n -> (column, n))
        (range, adjust) =
            case direction of
                Before -> ([boardMin .. (limit - 1)], reverse)
                After  -> ([(limit + 1) .. boardMax], id)
    in adjust . letters . adjust $ map makePosition range
    where letters = map tileToLetter . map fromJust . takeWhile isJust . map (getTile board)
{-
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
-}
getWordFromTilePlacement :: Board -> Orientation -> TilePlacement -> Maybe Word
getWordFromTilePlacement board orientation tilePlacement =
    let before = getLetters board tilePlacement orientation Before
        placedLetter = pure $ tilePlacementToLetter tilePlacement
        after  = getLetters board tilePlacement orientation After
        word = Word $ before ++ placedLetter ++ after
    in if null before && null after
       then Nothing 
       else Just word 
{-
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
-}
