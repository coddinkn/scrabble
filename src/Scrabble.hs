module Scrabble where

import Board
import Player

import Control.Monad.State
import Control.Monad.Except
import Data.List.Ordered
import Data.List

data GameState = GameState { players :: Players
                           , board   :: Board
                           , tiles   :: Tiles
                           }

type MonadScrabble m = (MonadError String m, MonadState GameState m)

newtype Scrabble a = Scrabble {
    runScrabble :: ExceptT String (StateT GameState IO) a
} deriving (Functor, Applicative, Monad, 
            MonadError String, 
            MonadState GameState,
            MonadIO)

startTiles :: Tiles
startTiles = replicate 100 $ Tile 'A'

newGame :: GameState
newGame = GameState [] emptyBoard startTiles

addPlayer :: MonadScrabble m => String -> m ()
addPlayer ipAddress = do gameState <- get
                         existingPlayers <- gets players
                         let newPlayer = Player ipAddress [] 0
                         put $ gameState { players = newPlayer : existingPlayers} 

changeUsername :: MonadScrabble m => String -> String -> m ()
changeUsername old new = do player <- getPlayer old
                            return ()

getPlayer :: MonadScrabble m => String -> m Player
getPlayer uname = do players <- gets players 
                     let maybePlayer = find sameUsername players 
                     maybe (throwError "Player does not exist!")
                              return
                              maybePlayer 
    where sameUsername player = uname == username player

playTiles :: MonadScrabble m => String -> Tiles -> m ()
playTiles username tilesToPlay = do player <- getPlayer username
                                    if subset tilesToPlay $ Player.tiles player
                                    then return ()
                                    else throwError "Bad tiles!"

playScrabble :: Scrabble () -> IO ()
playScrabble game = do (result, _) <- flip runStateT newGame . runExceptT $ runScrabble game
                       either putStrLn
                              (const $ return ())
                              result
