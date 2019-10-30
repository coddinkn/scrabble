module ScrabbleT 
( ScrabbleT
, playScrabbleT
, addPlayer
, getScore
, setTurn 
, changeUsername
, getPlayer
, playTiles
) where

import Board
import Tile
import Player
import GameState hiding (addPlayer, getPlayer)
import qualified GameState as GS (addPlayer, getPlayer)

import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Data.List.Ordered

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

getScore :: Monad m => Username -> ScrabbleT m Int
getScore username = do player <- getPlayer username
                       return $ score player

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

getPlayer :: Monad m => Username -> ScrabbleT m Player
getPlayer username = do maybePlayer <- GS.getPlayer username <$> get
                        maybe noPlayerException
                              return
                              maybePlayer 
    where noPlayerException = throwError $ "player " ++ username ++ " does not exist"

playTiles :: Monad m => String -> Tiles -> ScrabbleT m ()
playTiles username tilesToPlay = do player <- getPlayer username
                                    if subset tilesToPlay $ Player.tiles player
                                    then return ()
                                    else throwError $ wrongTilesError tilesToPlay $ Player.tiles player
    where wrongTilesError toPlay tiles = show toPlay ++ " is not a subset of " ++ show tiles
