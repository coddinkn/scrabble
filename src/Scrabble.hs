module Scrabble where

import Board
import Player

import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Except
import Data.List.Ordered
import Data.List

data GameState = GameState { players  :: Players
                           , board    :: Board
                           , tiles    :: Tiles
                           , whosTurn :: Maybe Player
                           }

newtype Monad m => ScrabbleT m a = ScrabbleT {
    runScrabbleT :: ExceptT String (StateT GameState m) a
} deriving (Functor, Applicative, Monad, 
            MonadError String, 
            MonadState GameState)

instance MonadTrans ScrabbleT where
    lift = ScrabbleT . lift . lift

-- getScore :: Username -> m Int

startTiles :: Tiles
startTiles = replicate 100 $ Tile 'A'

newGame :: GameState
newGame = GameState [] emptyBoard startTiles Nothing

addPlayer :: Monad m => String -> ScrabbleT m ()
addPlayer ipAddress = do gameState <- get
                         existingPlayers <- gets players
                         let newPlayer = Player ipAddress [] 0
                         put $ gameState { players = newPlayer : existingPlayers} 

changeUsername :: Monad m => Username -> Username -> ScrabbleT m ()
changeUsername old new = do player <- getPlayer old
                            return ()

getPlayer :: Monad m => String -> ScrabbleT m Player
getPlayer uname = do players <- gets players 
                     let maybePlayer = find sameUsername players 
                     maybe (throwError "Player does not exist!")
                              return
                              maybePlayer 
    where sameUsername player = uname == username player

playTiles :: Monad m => String -> Tiles -> ScrabbleT m ()
playTiles username tilesToPlay = do player <- getPlayer username
                                    if subset tilesToPlay $ Player.tiles player
                                    then return ()
                                    else throwError "Bad tiles!"

playScrabbleT :: Monad m => ScrabbleT m () -> (String -> m ()) -> m ()
playScrabbleT game handler = do (result, _) <- flip runStateT newGame . runExceptT $ runScrabbleT game
                                either handler
                                       (const $ return ())
                                       result

instance MonadIO m => MonadIO (ScrabbleT m) where
    liftIO = lift . liftIO

type ScrabbleIO a = ScrabbleT IO a

playScrabbleIO :: ScrabbleIO () -> IO ()
playScrabbleIO = flip playScrabbleT putStrLn
