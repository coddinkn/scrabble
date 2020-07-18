module ScoreSpec where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Arbitrary

import Score

instance Arbitrary Score where
    arbitrary = Score <$> arbitrary <*> arbitrary

leftIdentity :: Score -> Bool
leftIdentity score = (mempty <> score) == score

rightIdentity :: Score -> Bool
rightIdentity score = (score <> mempty) == score

associativity :: (Score, Score, Score) -> Bool
associativity (x, y, z) = ((x <> y) <> z) == (x <> (y <> z))

computesCorrectly :: (Int, Int) -> Bool
computesCorrectly (multiplier, baseScore) = actual == expected
    where actual   = compute $ Score multiplier baseScore
          expected = multiplier * baseScore

spec :: Spec
spec = do
    describe "(<>)" $ do
        it "obeys the left identity law"  $ property leftIdentity
        it "obeys the right identity law" $ property rightIdentity
        it "obeys the associativity law"  $ property associativity
    describe "compute" $ do
        it "computes scores correctly" $ property computesCorrectly
