module ScrabbleT 
( ScrabbleT
, playScrabbleT
, addPlayer
, getScore
, changeUsername
, getBoard
, placeTiles
, ready
) where

import Board
import GameState (GameState, Username, modifyBoard, modifyPlayer)
import qualified GameState as GS
import Player (Player)
import qualified Player as P
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
playScrabbleT game handler words = do (result, _) <- flip runReaderT words $ flip runStateT GS.newGame . runExceptT $ runScrabbleT game
                                      either handler
                                             (const $ return ())
                                             result

ready :: Monad m => Username -> ScrabbleT m Bool
ready username =
    do playerExists <- gets $ GS.checkUsername username
       unless playerExists . throwError $ "Player " ++ username ++ " not found"
       modify $ GS.ready username
       started <- gets GS.started
       when started . modify $ GS.setTurn username
       return started

giveTiles :: Monad m => Int -> ScrabbleT m ()
giveTiles n = do username <- whosTurn
                 modify $ GS.givePlayerTiles username n

getScore :: Monad m => Username -> ScrabbleT m Int
getScore username = do playerExists <- gets $ GS.checkUsername username
                       unless playerExists . throwError $ "Player " ++ username ++ " not found"
                       gets $ GS.getFromPlayer username P.playerScore

changeScore :: Monad m => Username -> (Int -> Int) -> ScrabbleT m Int
changeScore username change =
       do modify $ modifyPlayer username $ P.changePlayerScore change
          getScore username

nextTurn :: Monad m => ScrabbleT m ()
nextTurn = modify GS.nextTurn

addPlayer :: Monad m => Username -> ScrabbleT m ()
addPlayer username = do modify $ GS.addPlayer username
                        modify $ GS.givePlayerTiles username 7

changeUsername :: Monad m => Username -> Username -> ScrabbleT m ()
changeUsername oldUsername newUsername = do playerExists <- gets $ GS.checkUsername oldUsername
                                            unless playerExists . throwError $ "Player " ++ oldUsername ++ " not found"
                                            modify $ GS.changeUsername oldUsername newUsername

getBoard :: Monad m => ScrabbleT m Board
getBoard = gets GS.board

whosTurn :: Monad m => ScrabbleT m Username
whosTurn = do maybeUsername <- gets GS.whosTurn
              case maybeUsername of
                  Just username -> return username
                  Nothing -> throwError $ "The game has not started"

placeTile :: Monad m => TilePlacement -> ScrabbleT m ()
placeTile = modify . modifyBoard . putTile

placeTiles :: Monad m => [TilePlacement] -> ScrabbleT m Int
placeTiles tilePlacements =
    do username <- whosTurn
       board <- getBoard
       dictionary <- ask
       inPlayerTiles <- gets $ GS.getFromPlayer username (P.inPlayerTiles tiles)
       unless inPlayerTiles $ do playerTiles <- gets $ GS.getFromPlayer username P.tiles
                                 throwError $ wrongTilesError playerTiles
       words <- liftEither $ getWords dictionary board tilePlacements
       mapM_ placeTile tilePlacements
       let addScore = (+) $ compute . mconcat $ map score words
       nextTurn
       changeScore username addScore
    where tiles = sort $ map tile tilePlacements
          positions = map position tilePlacements
          wrongTilesError playerTiles = show tiles ++ " is not a subset of " ++ show playerTiles

pass :: Monad m => ScrabbleT m ()
pass = do username <- whosTurn
          undefined

exchange :: Monad m => ScrabbleT m ()
exchange = do username <- whosTurn
              undefined
