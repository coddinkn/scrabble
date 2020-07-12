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

import Control.Monad (unless)
import Data.Either (isLeft, rights)
import Data.List (sortBy, find)
import Data.Maybe (fromJust, isJust, catMaybes)
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

newtype Word = Word [Letter]

instance Scorable Word where
    score (Word letters) = mconcat $ map score letters

instance Show Word where
    show (Word letters) = letters >>= show

data Direction = Before
               | After

type Dictionary = [String]

checkWord :: Dictionary -> Word -> Either String Word
checkWord dictionary word = do
    unless inDictionary . Left $ show word ++ " is not in the dictionary"
    return word
    where inDictionary = show word `elem` dictionary


getWords' :: Dictionary -> Board -> [TilePlacement] -> Either String [Word]

getWords' dictionary board [] = Left "No tiles placed"

getWords' dictionary board [tilePlacement]
    | isJust maybeVerticalWord   = Right [fromJust maybeVerticalWord]
    | isJust maybeHorizontalWord = Right [fromJust maybeHorizontalWord]
    | otherwise = Left "Tile not placed near other tiles!"
    where maybeVerticalWord   = getWordFromTilePlacement board Vertical   tilePlacement
          maybeHorizontalWord = getWordFromTilePlacement board Horizontal tilePlacement

getWords' dictionary board tilePlacements =
    case orientation $ position <$> tilePlacements of
      Just orientation ->
        let perpWords = catMaybes $ map getPerpWord tilePlacements
        in do word <- getWordFromTilePlacements dictionary board orientation tilePlacements
              let words = word:perpWords
              mapM (checkWord dictionary) words
        where getPerpWord = getWordFromTilePlacement board $ opposite orientation
      Nothing -> Left "Tiles not placed in single row or column"

crossesCenter :: Word -> Bool
crossesCenter (Word letters) =
    elem (7, 7) . map position . rights $ map unLetter letters

containsLetterFromBoard :: Word -> Bool
containsLetterFromBoard (Word letters) = any fromBoard letters

checkIsFirstPlay :: [Word] -> Board -> Either String ()
checkIsFirstPlay words board
    | board /= emptyBoard = Left "you must include an existing letter in your word"
    | not $ any crossesCenter words = Left "the first word must cross through the center tile"
    | otherwise = return ()

getWords :: Dictionary -> Board -> [TilePlacement] -> Either String [Word]
getWords dictionary board tilePlacements = do
    words <- getWords' dictionary board tilePlacements
    unless (any containsLetterFromBoard words) $ checkIsFirstPlay words board
    return words

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

getWordFromTilePlacement :: Board -> Orientation -> TilePlacement -> Maybe Word
getWordFromTilePlacement board orientation tilePlacement =
    let before = getLetters board tilePlacement orientation Before
        placedLetter = pure $ tilePlacementToLetter tilePlacement
        after  = getLetters board tilePlacement orientation After
        word = Word $ before ++ placedLetter ++ after
    in if null before && null after
       then Nothing 
       else Just word 

collisionMessage :: TilePlacement -> Tile -> String
collisionMessage tilePlacement tile =
    placed ++ " being placed at " ++ posn ++ " where " ++ already ++ " was already placed"
    where placed = show tilePlacement
          posn = show $ position tilePlacement
          already = show tile

getLetterBetween :: Board -> [TilePlacement] -> Position -> Either String Letter
getLetterBetween board tilePlacements posn =
    let maybeTilePlacement = find sameSpot tilePlacements
        maybeTile = getTile board posn
    in case maybeTilePlacement of
        Just tilePlacement ->
            case maybeTile of
                Just tile -> Left  $ collisionMessage tilePlacement tile
                Nothing   -> Right $ tilePlacementToLetter tilePlacement
        Nothing ->
            case maybeTile of
                Just tile -> Right $ tileToLetter tile
                Nothing   -> Left "The tiles being placed to not bridge existing tiles"
    where sameSpot tilePlacement = position tilePlacement == posn

getLettersBetween :: Board -> [TilePlacement] -> Orientation -> Either String [Letter]
getLettersBetween board tilePlacements orientation =
    let positions = map position tilePlacements
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

getWordFromTilePlacements :: Dictionary -> Board -> Orientation -> [TilePlacement] -> Either String Word
getWordFromTilePlacements dictionary board orientation tilePlacements =
    let before = getLetters board (head sorted) orientation Before
        after  = getLetters board (last sorted) orientation After
    in do between <- getLettersBetween board sorted orientation
          return . Word $ before ++ between ++ after
    where sorted = sortBy (lineUp orientation) tilePlacements
