import Scrabble
import Board
import Player

import Control.Monad.Except

killian = Player "127.0.0.1" []  0

scrabble :: ScrabbleIO ()
scrabble = flip catchError handler $ do addPlayer "knc"
                                        playTiles "dmr" []
    where handler error = do liftIO $ putStrLn error
                             playTiles "knc" [ Tile 'B' ]

main :: IO ()
main = playScrabbleIO scrabble
