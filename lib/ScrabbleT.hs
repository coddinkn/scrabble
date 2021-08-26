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
import Scrabble.GameState (GameState, WaitingState, InProgressState, OverState, Username, modifyBoard, modifyPlayer)
import qualified Scrabble.GameState as GS
import Scrabble.Player
import Scrabble.Tile
import Scrabble.TilePlacement
import Scrabble.Score
import Scrabble.Word
import Scrabble.Exception

import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Data.List
import Data.Maybe

newtype ScrabbleT m a = ScrabbleT {
    runScrabbleT :: ExceptT Exception (StateT GameState (ReaderT [String] m)) a
} deriving (Functor, Applicative, Monad, 
            MonadError Exception,
            MonadReader [String],
            MonadState GameState)

instance MonadTrans ScrabbleT where
    lift = ScrabbleT . lift . lift . lift

instance MonadIO m => MonadIO (ScrabbleT m) where
    liftIO = lift . liftIO

playScrabbleT :: Monad m => [String] -> GameState -> ScrabbleT m () -> m (Either Exception GameState)
playScrabbleT dictionary gameState scrabble = do
    (result, newGameState) <- flip runReaderT dictionary . flip runStateT gameState . runExceptT $ runScrabbleT scrabble
    return $ const newGameState <$> result

getFromWaiting :: Monad m => (WaitingState -> a) -> ScrabbleT m a
getFromWaiting f = do gameState <- get
                      case gameState of
                          GS.Waiting state -> return $ f state
                          _ -> throwError $ IncorrectState "Must be in waiting"

modifyWaiting :: Monad m => (WaitingState -> GameState) -> ScrabbleT m ()
modifyWaiting f = getFromWaiting f >>= put

getFromInProgress :: Monad m => (InProgressState -> a) -> ScrabbleT m a
getFromInProgress f = do gameState <- get
                         case gameState of
                             GS.InProgress state -> return $ f state
                             _ -> throwError $ IncorrectState "Must be in progress"

modifyInProgress :: Monad m => (InProgressState -> GameState) -> ScrabbleT m ()
modifyInProgress f = getFromInProgress f >>= put

getFromOver :: Monad m => (OverState -> a) -> ScrabbleT m a
getFromOver f = do gameState <- get
                   case gameState of
                       GS.Over state -> return $ f state
                       _ -> throwError $ IncorrectState "Must be over"

modifyOver :: Monad m => (OverState -> GameState) -> ScrabbleT m ()
modifyOver f = getFromOver f >>= put

-- just for testing shenanigans
readyWithTiles :: Monad m => Username -> [Tile] -> ScrabbleT m Bool
readyWithTiles username startTiles = do
    playerExists <- gets $ GS.checkUsername username
    unless playerExists . throwError . Generic $ "Player " ++ username ++ " not found"
    modifyWaiting $ GS.readyUserWithTiles startTiles username
    do { users <- getFromInProgress GS.getUsers;
         mapM_ (\user -> modifyInProgress $ GS.InProgress . GS.giveUserTiles 7 user) users;
         return True;
       } `catchError` handler
    where handler error = case error of
                              IncorrectState _ -> return False
                              _ -> throwError error
          handler :: Monad m => Exception -> ScrabbleT m Bool

ready :: Monad m => Username -> ScrabbleT m Bool
ready username = do
    playerExists <- gets $ GS.checkUsername username
    unless playerExists . throwError . Generic $ "Player " ++ username ++ " not found"
    modifyWaiting $ GS.readyUser username
    do { users <- getFromInProgress GS.getUsers;
         mapM_ (\user -> modifyInProgress $ GS.InProgress . GS.giveUserTiles 7 user) users;
         return True;
       } `catchError` handler
    where handler error = case error of
                              IncorrectState _ -> return False
                              _ -> throwError error
          handler :: Monad m => Exception -> ScrabbleT m Bool

giveTiles :: Monad m => Int -> ScrabbleT m ()
giveTiles n = do
    username <- whosTurn
    modifyInProgress $ GS.InProgress . GS.giveUserTiles n username

getScore :: Monad m => Username -> ScrabbleT m Int
getScore username = do
    maybeScore <- gets $ GS.getFromPlayer username playerScore
    case maybeScore of
        Just score -> return score
        Nothing -> throwError . Generic $ "Player " ++ username ++ " not found"

changeScore :: Monad m => Username -> (Int -> Int) -> ScrabbleT m Int
changeScore username change = do
    modify $ modifyPlayer username $ changePlayerScore change
    getScore username

addPlayer :: Monad m => Username -> ScrabbleT m ()
addPlayer username = do
    playerExists <- gets $ GS.checkUsername username
    when playerExists . throwError . Generic $ "Player " ++ username ++ " already exists"
    modifyWaiting $ GS.Waiting . GS.addUser username

changeUsername :: Monad m => Username -> Username -> ScrabbleT m ()
changeUsername oldUsername newUsername = do
    playerExists <- gets $ GS.checkUsername oldUsername
    unless playerExists . throwError . Generic $ "Player " ++ oldUsername ++ " not found"
    modify $ GS.changeUsername oldUsername newUsername

getBoard :: Monad m => ScrabbleT m Board
getBoard = getFromInProgress GS.getBoard

whosTurn :: Monad m => ScrabbleT m Username
whosTurn = getFromInProgress GS.whosTurn

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
    modifyInProgress $ GS.InProgress . GS.nextTurn
    changeScore username addScore
    where tiles = sort $ map tile tilePlacements
          placeTile = modify . modifyBoard . putTile
          wrongTilesError playerTiles = Generic $ show tiles ++ " is not a subset of " ++ show playerTiles

pass :: Monad m => ScrabbleT m ()
pass = do
    username <- whosTurn
    passedLastTurn <- fmap fromJust . gets $ GS.getFromPlayer username passedLastTurn
    if passedLastTurn
        then modifyInProgress $ GS.Over . GS.endGame
        else modify $ GS.modifyPlayer username markPass

exchange :: Monad m => Maybe Tile -> ScrabbleT m ()
exchange maybeTile = do
    username <- whosTurn
    undefined
