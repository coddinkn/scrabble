import Scrabble

import Control.Monad.Trans

scrabble :: ScrabbleIO ()
scrabble = do addPlayer "dmr"
              addPlayer "knc"
              giveTiles "dmr" 7
              giveTiles "knc" 7
              placeTiles "dmr" [ flip TilePlacement (7, 6) $ Tile 'B'
                               , flip TilePlacement (7, 7) $ Tile 'A'
                               , flip TilePlacement (7, 8) $ Tile 'T'
                               ] >>= liftIO . print
              getBoard >>= liftIO . print
              placeTiles "dmr" [ flip TilePlacement (6, 7) $ Tile 'C'
                               , flip TilePlacement (8, 7) $ Tile 'T'
                               ] >>= liftIO . print
              getBoard >>= liftIO . print
              placeTiles "knc" [ flip TilePlacement (9, 7) $ Tile 'S' ] >>= liftIO . print
              getBoard >>= liftIO . print
              getScore "knc" >>= liftIO . print
              getScore "dmr" >>= liftIO . print


main :: IO ()
main = let words = [ "BAT"
                   , "CAT"
                   , "CATS"
                   ]
       in playScrabbleIO scrabble words
