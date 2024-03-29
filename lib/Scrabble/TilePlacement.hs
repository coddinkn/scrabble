module Scrabble.TilePlacement
( TilePlacement (..)
, lineUp
) where

import Scrabble.Modifier
import Scrabble.Position
import Scrabble.Score
import Scrabble.Tile

data TilePlacement = TilePlacement { tile :: Tile
                                   , position :: Position
                                   } deriving Eq

modifierToScoreBuilder :: Modifier -> (Int -> (Int, Int))
modifierToScoreBuilder DoubleLetter = \score -> (1, 2 * score)
modifierToScoreBuilder TripleLetter = \score -> (1, 3 * score)
modifierToScoreBuilder DoubleWord   = (2, )
modifierToScoreBuilder TripleWord   = (3, )

instance Scorable TilePlacement where
    score placement = 
        let scoreBuilder =
                maybe (1, )
                      modifierToScoreBuilder
                    $ modifier $ position placement  
            (multiplier, baseScore) = scoreBuilder . compute . score $ tile placement
        in Score multiplier baseScore

instance Show TilePlacement where
    show tilePlacement = show $ tile tilePlacement

lineUp :: Orientation -> (TilePlacement -> TilePlacement -> Ordering)
lineUp orientation =
    let select = case orientation of
                    Vertical   -> snd . position
                    Horizontal -> fst . position
    in \a b -> compare (select a) (select b)
