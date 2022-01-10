module BugSpec where

import Data.List.NonEmpty (fromList)

import Test.Hspec
import Test.Hspec.Expectations

import Scrabble
    ( evalScrabble
    , addPlayer
    , readyWithTiles
    , ready
    , placeTiles
    )
import Scrabble.Tile          (Tile(..))
import Scrabble.TilePlacement (TilePlacement(..))
import Scrabble.GameState     (newGame)

import Control.Monad.IO.Class (liftIO)
import System.Random

tilePlacement :: Char -> (Int, Int) -> TilePlacement
tilePlacement c pos = TilePlacement (Tile c) pos

playerA = "A"
playerB = "B"

bug_2021_12_31 :: StdGen -> Expectation
bug_2021_12_31 gen =
    let bratScore = evalScrabble ["DRONE", "BRAT", "RANT"] (newGame gen) $ do
            addPlayer playerA
            addPlayer playerB
            ready playerA
            readyWithTiles playerB . map Tile $ concat ["DRONERA", "BATZZZ", "ZZZZZ", "TZZZZZZZZZZZZ"]
            placeTiles . fromList $
                zipWith tilePlacement "DRONE" [(6, 7), (7, 7), (8, 7), (9, 7), (10, 7)]
            placeTiles . fromList $
               zipWith tilePlacement "BAT" [(7, 6), (7, 8), (7, 9)]
            placeTiles . fromList $
               zipWith tilePlacement "RAT" [(9, 5), (9, 6), (9, 8)]
    in  either (expectationFailure . show) (`shouldBe` 12) bratScore

spec :: Spec
spec = beforeAll getStdGen $ do
    describe "2021-12-31 bug" $ do
        it "should result in valid score" bug_2021_12_31
