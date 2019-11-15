import Scrabble

import Control.Monad.Trans

scrabble :: ScrabbleIO ()
scrabble = do addPlayer "dmr"
              giveTiles "dmr" 7
              placeTiles "dmr" [ flip TilePlacement (7, 6) $ Tile 'B'
                               , flip TilePlacement (7, 7) $ Tile 'A'
                               , flip TilePlacement (7, 8) $ Tile 'T'
                               ] >>= liftIO . print
              getBoard >>= liftIO . print
              placeTiles "dmr" [ flip TilePlacement (6, 7) $ Tile 'C'
                               , flip TilePlacement (8, 7) $ Tile 'T'
                               ] >>= liftIO . print
              getBoard >>= liftIO . print

main :: IO ()
main = let words = [ "BAT"
                   , "CAT"
                   , "CATS"
                   ]
       in playScrabbleIO scrabble words
