import Scrabble

import Control.Monad.Except

scrabble :: ScrabbleIO ()
scrabble = flip catchError handler $ do addPlayer "knc"
                                        playTiles "dmr" []
    where handler error = do liftIO $ putStrLn error
                             changeUsername "knc" "dmr"
                             playTiles "dmr" [ Tile 'B' ]

main :: IO ()
main = let words = ["test"]
       in playScrabbleIO scrabble words
