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
import Scrabble.GameState ( GameState(..)
                          , WaitingState
                          , InProgressState
                          , OverState
                          , Username
                          , modifyBoard
                          , modifyPlayer)
import qualified Scrabble.GameState as GS
import MonadScrabble
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
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe

import Lens.Micro ((^.), (.~), (%~), (&))

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
                          Waiting state -> return $ f state
                          _ -> throwError $ IncorrectState "Must be in waiting"

modifyWaiting :: Monad m => (WaitingState -> GameState) -> ScrabbleT m ()
modifyWaiting f = getFromWaiting f >>= put

getFromInProgress :: Monad m => (InProgressState -> a) -> ScrabbleT m a
getFromInProgress f = do gameState <- get
                         case gameState of
                             InProgress state -> return $ f state
                             _ -> throwError $ IncorrectState "Must be in progress"

getFromPlayer :: Monad m => Username -> (Player -> a) -> ScrabbleT m a
getFromPlayer username f = (getFromInProgress $ GS.getFromPlayer username f) >>= liftEither

modifyInProgress :: Monad m => (InProgressState -> GameState) -> ScrabbleT m ()
modifyInProgress f = getFromInProgress f >>= put

getFromOver :: Monad m => (OverState -> a) -> ScrabbleT m a
getFromOver f = do gameState <- get
                   case gameState of
                       Over state -> return $ f state
                       _ -> throwError $ IncorrectState "Must be over"

modifyOver :: Monad m => (OverState -> GameState) -> ScrabbleT m ()
modifyOver f = getFromOver f >>= put

instance (Monad m) => MonadScrabble (ScrabbleT m) where

    -- just for testing shenanigans
    readyWithTiles username startTiles = do
        playerExists <- gets $ GS.checkUsername username
        unless playerExists . throwError $ UnknownUser username
        modifyWaiting $ GS.readyUserWithTiles startTiles username
        do { users <- getFromInProgress GS.getUsers;
             mapM_ (\user -> modifyInProgress $ InProgress . GS.giveUserTiles 7 user) users;
             return True;
           } `catchError` handler
        where handler error = case error of
                                  IncorrectState _ -> return False
                                  _ -> throwError error
              handler :: Monad m => Exception -> ScrabbleT m Bool

    ready username = do
        playerExists <- gets $ GS.checkUsername username
        unless playerExists . throwError $ UnknownUser username
        modifyWaiting $ GS.readyUser username
        do { users <- getFromInProgress GS.getUsers;
             mapM_ (\user -> modifyInProgress $ InProgress . GS.giveUserTiles 7 user) users;
             return True;
           } `catchError` handler
        where handler error = case error of
                                  IncorrectState _ -> return False
                                  _ -> throwError error
              handler :: Monad m => Exception -> ScrabbleT m Bool

    giveTiles n = do
        username <- whosTurn
        modifyInProgress $ InProgress . GS.giveUserTiles n username

    getScore = flip getFromPlayer (^. playerScore)

    changeScore username change = do
        modifyInProgress $ InProgress . GS.modifyPlayer username (playerScore %~ change)
        getScore username

    addPlayer username = do
        playerExists <- gets $ GS.checkUsername username
        when playerExists . throwError $ UsernameTaken username
        modifyWaiting $ Waiting . GS.addUser username

    changeUsername oldUsername newUsername = do
        playerExists <- gets $ GS.checkUsername oldUsername
        unless playerExists . throwError $ UnknownUser oldUsername
        get >>= liftEither . GS.changeUsername oldUsername newUsername >>= put

    getBoard = getFromInProgress GS.getBoard

    whosTurn = getFromInProgress GS.whosTurn

    placeTiles tilePlacements = do
        username <- whosTurn
        board <- getBoard
        dictionary <- ask
        inPlayerTiles <- getFromPlayer username (inPlayerTiles tiles)
        unless inPlayerTiles $ do
            playerTiles <- getFromPlayer username (^. playerTiles)
            throwError $ wrongTilesError playerTiles
        words <- liftEither $ getWords dictionary board tilePlacements
        mapM_ placeTile tilePlacements
        let addScore = (+) $ compute . mconcat $ map score words
        modifyInProgress $ InProgress . GS.nextTurn
        changeScore username addScore
        where tiles = NE.sort $ fmap tile tilePlacements
              placeTile tp = modifyInProgress $ InProgress . modifyBoard (putTile tp)
              wrongTilesError playerTiles = IncorrectTiles (show tiles) (show playerTiles)

    pass = do
        username <- whosTurn
        passed <- getFromPlayer username (^. passedLastTurn)
        if passed
            then modifyInProgress $ Over . GS.endGame
            else modifyInProgress $ InProgress . GS.modifyPlayer username (passedLastTurn .~ True)

    exchange maybeTile = do
        username <- whosTurn
        undefined
