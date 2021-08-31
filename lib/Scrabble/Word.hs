module Scrabble.Word 
( Word
, Dictionary
, getWords
) where

import Scrabble.Board
import Scrabble.Position
import Scrabble.Tile
import Scrabble.TilePlacement
import Scrabble.Score
import Scrabble.Exception

import Control.Monad (unless)
import Data.Either (isLeft, rights)
import Data.List (sortBy, find)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromJust, isJust, mapMaybe)
import Data.Semigroup (sconcat)
import Prelude hiding (Word)

newtype Letter = Letter (Either Tile TilePlacement)
instance Scorable Letter where
    score (Letter value) = either score score value

instance Show Letter where
    show (Letter value) = either show show value

tileToLetter :: Tile -> Letter
tileToLetter = Letter . Left

unLetter :: Letter -> Either Tile TilePlacement
unLetter (Letter value) = value

tilePlacementToLetter :: TilePlacement -> Letter
tilePlacementToLetter = Letter . Right

fromBoard :: Letter -> Bool
fromBoard = isLeft . unLetter

newtype Word = Word (NonEmpty Letter)

instance Scorable Word where
    score (Word letters) = sconcat $ fmap score letters

instance Show Word where
    show (Word letters) = NE.toList letters >>= show

data Direction = Before
               | After

type Dictionary = [String]

checkWord :: Dictionary -> Word -> Either Exception Word
checkWord dictionary word = do
    unless inDictionary . Left . InvalidWord $ show word
    return word
    where inDictionary = show word `elem` dictionary

getPotentialWords :: Dictionary -> Board -> NonEmpty TilePlacement -> Either Exception [Word]

getPotentialWords _ board (tilePlacement :| [])
    | isJust maybeVerticalWord   = Right [fromJust maybeVerticalWord]
    | isJust maybeHorizontalWord = Right [fromJust maybeHorizontalWord]
    | otherwise = Left $ ImproperTilePlacement "Tile not placed near other tiles!"
    where maybeVerticalWord   = getWordFromTilePlacement board Vertical   tilePlacement
          maybeHorizontalWord = getWordFromTilePlacement board Horizontal tilePlacement

getPotentialWords dictionary board tilePlacements =
    case orientation . NE.toList $ position <$> tilePlacements of
      Just orientation ->
        let perpWords = mapMaybe getPerpWord $ NE.toList tilePlacements
        in do word <- getWordFromTilePlacements board orientation tilePlacements
              let words = word:perpWords
              mapM (checkWord dictionary) words
        where getPerpWord = getWordFromTilePlacement board $ opposite orientation
      Nothing -> Left $ ImproperTilePlacement "Tiles not placed in single row or column"

crossesCenter :: Word -> Bool
crossesCenter (Word letters) =
    elem (7, 7) . map position . rights . map unLetter $ NE.toList letters

containsLetterFromBoard :: Word -> Bool
containsLetterFromBoard (Word letters) = any fromBoard letters

checkIsFirstPlay :: [Word] -> Board -> Either Exception ()
checkIsFirstPlay words board
    | board /= emptyBoard = Left $ ImproperTilePlacement "Must include an existing letter in your word"
    | not $ any crossesCenter words = Left $ ImproperTilePlacement "the first word must cross through the center tile"
    | otherwise = return ()

getWords :: Dictionary -> Board -> NonEmpty TilePlacement -> Either Exception [Word]
getWords dictionary board tilePlacements = do
    potentialWords <- getPotentialWords dictionary board tilePlacements
    unless (any containsLetterFromBoard potentialWords) $ checkIsFirstPlay potentialWords board
    return potentialWords

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
    where letters = map (tileToLetter . fromJust) . takeWhile isJust . map (getTile board)

getWordFromTilePlacement :: Board -> Orientation -> TilePlacement -> Maybe Word
getWordFromTilePlacement board orientation tilePlacement =
    let before = getLetters board tilePlacement orientation Before
        placedLetter = pure $ tilePlacementToLetter tilePlacement
        after  = getLetters board tilePlacement orientation After
    in if null before && null after
       then Nothing 
       else let maybeLetters = NE.nonEmpty $ before ++ placedLetter ++ after
            in Word <$> maybeLetters

collisionException :: TilePlacement -> Tile -> Exception
collisionException tilePlacement tile = ImproperTilePlacement $
    placed ++ " being placed at " ++ posn ++ " where " ++ already ++ " was already placed"
    where placed = show tilePlacement
          posn = show $ position tilePlacement
          already = show tile

getLetterBetween :: Board -> NonEmpty TilePlacement -> Position -> Either Exception Letter
getLetterBetween board tilePlacements posn =
    let maybeTilePlacement = find sameSpot tilePlacements
        maybeTile = getTile board posn
    in case maybeTilePlacement of
        Just tilePlacement ->
            case maybeTile of
                Just tile -> Left $ collisionException tilePlacement tile
                Nothing   -> Right $ tilePlacementToLetter tilePlacement
        Nothing ->
            case maybeTile of
                Just tile -> Right $ tileToLetter tile
                Nothing   -> Left $ ImproperTilePlacement "The tiles being placed to not bridge existing tiles"
    where sameSpot tilePlacement = position tilePlacement == posn

getLettersBetween :: Board -> NonEmpty TilePlacement -> Orientation -> Either Exception [Letter]
getLettersBetween board tilePlacements orientation =
    let positions = NE.toList $ fmap position tilePlacements
        (constant, selector) =
            case orientation of
                Vertical   -> (snd $ head positions, fst)
                Horizontal -> (fst $ head positions, snd)
        max = maximum $ map selector positions
        min = minimum $ map selector positions
        betweenPositions =
            case orientation of
                Vertical   -> [min .. max] `zip` repeat constant
                Horizontal -> repeat constant `zip` [min .. max]
    in mapM getLetter betweenPositions
    where getLetter = getLetterBetween board tilePlacements

getWordFromTilePlacements :: Board -> Orientation -> NonEmpty TilePlacement -> Either Exception Word
getWordFromTilePlacements board orientation tilePlacements =
    let before = getLetters board (NE.head sorted) orientation Before
        after  = getLetters board (NE.last sorted) orientation After
    in do between <- getLettersBetween board sorted orientation
          return . Word . NE.fromList $ before ++ between ++ after
    where sorted = NE.sortBy (lineUp orientation) tilePlacements
