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
import Data.List
import Data.Maybe

newtype ScrabbleT m a = ScrabbleT {
    runScrabbleT :: RandT StdGen (ExceptT String (StateT GameState (ReaderT [String] m))) a
} deriving (Functor, Applicative, Monad, 
            MonadError String, 
            MonadReader [String],
            MonadState GameState,
            MonadRandom)

instance MonadTrans ScrabbleT where
    lift = ScrabbleT . lift . lift . lift . lift

instance MonadIO m => MonadIO (ScrabbleT m) where
    liftIO = lift . liftIO

playScrabbleT :: Monad m => StdGen -> ScrabbleT m () -> (String -> m ()) -> [String] -> m ()
playScrabbleT generator game handler words = do
    (result, _) <- flip runReaderT words $ flip runStateT GS.newGame . runExceptT . flip evalRandT generator $ runScrabbleT game
    either handler
           (const $ return ())
           result

-- just for testing shenanigans
readyWithTiles :: Monad m => Username -> [Tile] -> ScrabbleT m Bool
readyWithTiles username startTiles = do
    playerExists <- gets $ GS.checkUsername username
    unless playerExists . throwError $ "Player " ++ username ++ " not found"
    modify $ GS.readyUser startTiles username
    started <- (== GS.Started) <$> gets GS.getStatus
    when started $ do
        users <- gets GS.turnOrder
        mapM_ (modify . GS.giveUserTiles 7) users
    return started

ready :: Monad m => Username -> ScrabbleT m Bool
ready username = do
    startTiles <- getStartTiles
    playerExists <- gets $ GS.checkUsername username
    unless playerExists . throwError $ "Player " ++ username ++ " not found"
    modify $ GS.readyUser startTiles username
    started <- (== GS.Started) <$> gets GS.getStatus
    when started $ do
        users <- gets GS.turnOrder
        mapM_ (modify . GS.giveUserTiles 7) users
    return started

shuffle :: Monad m => [a] -> ScrabbleT m [a]
shuffle x =
    if length x < 2
    then return x
    else do
        i <- getRandomR (0, length x - 1)
        r <- shuffle $ take i x ++ drop (i + 1) x
        return $ x !! i : r

getStartTiles :: Monad m => ScrabbleT m [Tile]
getStartTiles = do
    let regularTiles =
            concat $ map (map Tile . uncurry replicate)
                [ (9, 'A')
                , (2, 'B')
                , (2, 'C')
                , (4, 'D')
                , (12,'E')
                , (2, 'F')
                , (3, 'G')
                , (2, 'H')
                , (9, 'I')
                , (1, 'J')
                , (1, 'K')
                , (4, 'L')
                , (2, 'M')
                , (6, 'N')
                , (8, 'O')
                , (2, 'P')
                , (1, 'Q')
                , (6, 'R')
                , (4, 'S')
                , (6, 'T')
                , (4, 'U')
                , (2, 'V')
                , (2, 'W')
                , (1, 'X')
                , (2, 'Y')
                , (1, 'Z') ]
    first  <- Tile <$> getRandomR ('A', 'Z')
    second <- Tile <$> getRandomR ('A', 'Z')
    shuffle $ regularTiles ++ [first, second]

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
addPlayer username = modify $ GS.addUser username

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
        _ -> throwError $ "The game has not started"

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
