module ScrabbleT 
( ScrabbleT
, playScrabbleT
, addPlayer
, getScore
, setTurn 
, changeUsername
, giveTiles
, getPlayer
, getBoard
, placeTiles
) where

import Board
import GameState hiding (addPlayer, getPlayer)
import qualified GameState as GS (addPlayer, getPlayer, givePlayerTiles)
import Player
import Tile
import TilePlacement
import Score
import Word

import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Data.List
import Data.Maybe

newtype ScrabbleT m a = ScrabbleT {
    runScrabbleT :: ExceptT String (StateT GameState (ReaderT [String] m)) a
} deriving (Functor, Applicative, Monad, 
            MonadError String, 
            MonadReader [String],
            MonadState GameState)

instance MonadTrans ScrabbleT where
    lift = ScrabbleT . lift . lift . lift

instance MonadIO m => MonadIO (ScrabbleT m) where
    liftIO = lift . liftIO

playScrabbleT :: Monad m => ScrabbleT m () -> (String -> m ()) -> [String] -> m ()
playScrabbleT game handler words = do (result, _) <- flip runReaderT words $ flip runStateT newGame . runExceptT $ runScrabbleT game
                                      either handler
                                             (const $ return ())
                                             result

giveTiles :: Monad m => Username -> Int -> ScrabbleT m ()
giveTiles username n = do player <- getPlayer username
                          modify $ GS.givePlayerTiles player n

getScore :: Monad m => Username -> ScrabbleT m Int
getScore username = playerScore <$> getPlayer username

changeScore :: Monad m => Player -> (Int -> Int) -> ScrabbleT m Int
changeScore player change = do modify $ modifyPlayer player $ changePlayerScore change
                               getScore $ Player.username player

setTurn :: Monad m => Username -> ScrabbleT m ()
setTurn username = do gameState <- get
                      player <- getPlayer username
                      put $ gameState { whosTurn = Just player }

addPlayer :: Monad m => Username -> ScrabbleT m ()
addPlayer username = modify $ GS.addPlayer username

changeUsername :: Monad m => Username -> Username -> ScrabbleT m ()
changeUsername oldUsername newUsername =
    do player <- getPlayer oldUsername
       modify $ modifyPlayer player $ changePlayerUsername newUsername

getBoard :: Monad m => ScrabbleT m Board
getBoard = gets board

getPlayer :: Monad m => Username -> ScrabbleT m Player
getPlayer username = do maybePlayer <- gets $ GS.getPlayer username
                        maybe noPlayerException
                              return
                              maybePlayer 
    where noPlayerException = throwError $ "Player " ++ username ++ " does not exist"

placeTile :: Monad m => TilePlacement -> ScrabbleT m ()
placeTile = modify . modifyBoard . putTile

placeTiles :: Monad m => String -> [TilePlacement] -> ScrabbleT m Int
placeTiles username tilePlacements =
    do player <- getPlayer username
       board <- getBoard
       dictionary <- ask
       unless (tiles `isSubsequenceOf` Player.tiles player) (throwError . wrongTilesError $ Player.tiles player)
       words <- liftEither $ getWords dictionary board tilePlacements
       mapM_ placeTile tilePlacements
       let addScore = (+) $ compute . mconcat $ map score words
       changeScore player addScore
    where tiles = sort $ map tile tilePlacements
          positions = map position tilePlacements
          wrongTilesError playerTiles = show tiles ++ " is not a subset of " ++ show playerTiles
