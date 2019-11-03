import Scrabble

import Control.Monad.Except

scrabble :: ScrabbleIO ()
scrabble = flip catchError handler $ do addPlayer "knc"
                                        void $ placeTiles "dmr" []
    where handler error = do liftIO $ putStrLn error
                             changeUsername "knc" "dmr"
                             giveTiles "dmr" 1
                             score <- placeTiles "dmr" [ (Tile 'A', (3, 3)) ]
                             liftIO $ print score

main :: IO ()
main = let words = ["A"]
       in playScrabbleIO scrabble words
