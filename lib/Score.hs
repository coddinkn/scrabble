module Score
( Score (..)
, Scorable (..)
, compute
) where

data Score = Score Int Int
    deriving (Eq, Show)

instance Semigroup Score where
    (Score aMul aScore) <> (Score bMul bScore) =
        Score (aMul * bMul) (aScore + bScore)

instance Monoid Score where
    mempty = Score 1 0

compute :: Score -> Int
compute (Score multiplier baseScore) = multiplier * baseScore

class Scorable a where
    score :: a -> Score
