import Scrabble

import Control.Monad.Trans

scrabble :: ScrabbleIO ()
scrabble = do addPlayer "dmr"
              addPlayer "knc"
              ready "knc"
              ready "dmr"
              getBoard >>= liftIO . print
              placeTiles [ flip TilePlacement (7, 6) $ Tile 'B'
                         , flip TilePlacement (7, 7) $ Tile 'A'
                         , flip TilePlacement (7, 8) $ Tile 'T'
                         ] >>= liftIO . print
              getBoard >>= liftIO . print
              placeTiles [ flip TilePlacement (6, 7) $ Tile 'C'
                         , flip TilePlacement (8, 7) $ Tile 'T'
                         ] >>= liftIO . print
              getBoard >>= liftIO . print
              placeTiles [ flip TilePlacement (9, 7) $ Tile 'S' ] >>= liftIO . print
              getBoard >>= liftIO . print
              placeTiles [ flip TilePlacement (7, 9) $ Tile 'S' ] >>= liftIO . print
              getBoard >>= liftIO . print
              getScore "dmr" >>= liftIO . print
              getScore "knc" >>= liftIO . print


main :: IO ()
main = let words = [ "BAT"
                   , "CAT"
                   , "CATS"
                   , "BATS"
                   ]
       in playScrabbleIO scrabble words
