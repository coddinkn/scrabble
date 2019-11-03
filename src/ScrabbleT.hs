module ScrabbleT 
( ScrabbleT
, playScrabbleT
, addPlayer
, getScore
, setTurn 
, changeUsername
, giveTiles
, getPlayer
, placeTiles
) where

import Board
import Tile
import Player
import GameState hiding (addPlayer, getPlayer)
import qualified GameState as GS (addPlayer, getPlayer, givePlayerTiles)

import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Data.List.Ordered
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

getBoard :: Monad m => ScrabbleT m Board
getBoard = board <$> get

getPlayer :: Monad m => Username -> ScrabbleT m Player
getPlayer username = do maybePlayer <- GS.getPlayer username <$> get
                        maybe noPlayerException
                              return
                              maybePlayer 
    where noPlayerException = throwError $ "player " ++ username ++ " does not exist"

scoreWord :: Monad m => String -> ScrabbleT m Int
scoreWord word = do words <- ask
                    unless (word `elem` words) $ throwError $ word ++ " is not a valid word"
                    return 1

tallyScore :: Monad m => Int -> String -> ScrabbleT m Int
tallyScore score word = (+ score) <$> scoreWord word

placeTiles :: Monad m => String -> [(Tile, (Int, Int))] -> ScrabbleT m Int
placeTiles username placing =
    do player <- getPlayer username
       board <- getBoard
       unless (subset tiles $ Player.tiles player) (throwError $ wrongTilesError $ Player.tiles player)
       case orientation posns of
          Just direction -> let perpWords = filter (\word -> length word > 1) $ map (flip (getWordFromTile board) $ opposite direction) placing
                                allWords = (fromJust $ getWordFromTiles board placing) : perpWords
                            in foldM tallyScore 0 allWords
          Nothing -> throwError "Tiles not placed in a consecutive line"
    where tiles = map fst placing
          posns = map snd placing
          wrongTilesError playerTiles = show tiles ++ " is not a subset of " ++ show playerTiles
