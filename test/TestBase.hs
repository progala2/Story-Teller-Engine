{-# LANGUAGE FlexibleInstances #-}
module TestBase where

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Data.List

strings :: MonadGen m => Range.Range Int -> Range.Range Int -> m a -> m [[a]]
strings r1 r2 g = Gen.list r1 (Gen.list r2 g)
alphaStringNE :: MonadGen m => m [Char]
alphaStringNE = Gen.list (Range.linear 1 25) Gen.alpha
randSpace :: MonadGen m => m [Char]
randSpace = Gen.list (Range.linear 0 1) (return ' ')
spaces :: MonadGen m => (Int, Int) -> m [Char]
spaces (r1, r2) = Gen.list (Range.linear r1 r2) (return ' ')

sentence :: Monad m => (Int, Int) -> (Int, Int) -> GenT m [Char]
sentence = sentenceSp (1, 1)
sentenceSp :: Monad m => (Int, Int) -> (Int, Int) -> (Int, Int) -> GenT m [Char]
sentenceSp (sp1, sp2) (w1, w2) (s1, s2) = do
  nr <- Gen.int (Range.linear s1 s2)
  foldl' fld (Gen.list (Range.linear w1 w2) Gen.alpha) [1..nr]
  where
    fld a b = a <> spaces (sp1, sp2) <> (Gen.list (Range.linear w1 w2) Gen.alpha)