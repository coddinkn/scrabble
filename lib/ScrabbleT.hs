module ScrabbleT
( ScrabbleT
, playScrabbleT
, addPlayer
, getScore
, changeUsername
, getBoard
, placeTiles
, ready
, readyWithTiles -- shenanigans
, whosTurn
) where

import Scrabble.Board
import Scrabble.GameState (GameState, Username, modifyBoard, modifyPlayer)
import qualified Scrabble.GameState as GS
import Scrabble.Player
import Scrabble.Tile
import Scrabble.TilePlacement
import Scrabble.Score
import Scrabble.Word

import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Random
import Data.Bifunctor (second)
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

playScrabbleT :: Monad m => [String] -> GameState -> ScrabbleT m () -> m (Either String GameState)
playScrabbleT dictionary gameState scrabble = do
    (result, newGameState) <- flip runReaderT dictionary . flip runStateT gameState . runExceptT $ runScrabbleT scrabble
    return $ second (const newGameState) result

-- just for testing shenanigans
readyWithTiles :: Monad m => Username -> [Tile] -> ScrabbleT m Bool
readyWithTiles username startTiles = do
    playerExists <- gets $ GS.checkUsername username
    unless playerExists . throwError $ "Player " ++ username ++ " not found"
    modify $ GS.readyUserWithTiles startTiles username
    started <- (== GS.Started) <$> gets GS.getStatus
    when started $ do
        users <- gets GS.turnOrder
        mapM_ (modify . GS.giveUserTiles 7) users
    return started

ready :: Monad m => Username -> ScrabbleT m Bool
ready username = do
    playerExists <- gets $ GS.checkUsername username
    unless playerExists . throwError $ "Player " ++ username ++ " not found"
    modify $ GS.readyUser username
    started <- (== GS.Started) <$> gets GS.getStatus
    when started $ do
        users <- gets GS.turnOrder
        mapM_ (modify . GS.giveUserTiles 7) users
    return started

giveTiles :: Monad m => Int -> ScrabbleT m ()
giveTiles n = do
    username <- whosTurn
    modify $ GS.giveUserTiles n username

getScore :: Monad m => Username -> ScrabbleT m Int
getScore username = do
    maybeScore <- gets $ GS.getFromPlayer username playerScore
    case maybeScore of
        Just score -> return score
        Nothing -> throwError $ "Player " ++ username ++ " not found"

changeScore :: Monad m => Username -> (Int -> Int) -> ScrabbleT m Int
changeScore username change = do
    modify $ modifyPlayer username $ changePlayerScore change
    getScore username

addPlayer :: Monad m => Username -> ScrabbleT m ()
addPlayer username = do
    playerExists <- gets $ GS.checkUsername username
    when playerExists . throwError $ "Player " ++ username ++ " already exists"
    modify $ GS.addUser username

changeUsername :: Monad m => Username -> Username -> ScrabbleT m ()
changeUsername oldUsername newUsername = do
    playerExists <- gets $ GS.checkUsername oldUsername
    unless playerExists . throwError $ "Player " ++ oldUsername ++ " not found"
    modify $ GS.changeUsername oldUsername newUsername

getBoard :: Monad m => ScrabbleT m Board
getBoard = gets GS.board

whosTurn :: Monad m => ScrabbleT m Username
whosTurn = do
    status <- gets GS.getStatus
    case status of
        GS.Started -> gets GS.whosTurn
        _ -> throwError "The game has not started"

placeTiles :: Monad m => [TilePlacement] -> ScrabbleT m Int
placeTiles tilePlacements = do
    username <- whosTurn
    board <- getBoard
    dictionary <- ask
    inPlayerTiles <- fmap fromJust . gets $ GS.getFromPlayer username (inPlayerTiles tiles)
    unless inPlayerTiles $ do
        playerTiles <- gets $ GS.getFromPlayer username playerTiles
        throwError $ wrongTilesError playerTiles
    words <- liftEither $ getWords dictionary board tilePlacements
    mapM_ placeTile tilePlacements
    let addScore = (+) $ compute . mconcat $ map score words
    modify GS.nextTurn
    changeScore username addScore
    where tiles = sort $ map tile tilePlacements
          placeTile = modify . modifyBoard . putTile
          positions = map position tilePlacements
          wrongTilesError playerTiles = show tiles ++ " is not a subset of " ++ show playerTiles

pass :: Monad m => ScrabbleT m ()
pass = do
    username <- whosTurn
    passedLastTurn <- fmap fromJust . gets $ GS.getFromPlayer username passedLastTurn
    if passedLastTurn
        then modify GS.endGame
        else modify $ GS.modifyPlayer username markPass

exchange :: Monad m => Maybe Tile -> ScrabbleT m ()
exchange maybeTile = do
    username <- whosTurn
    undefined
