{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module TestBase where

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

strings :: MonadGen m => Range.Range Int -> Range.Range Int -> m a -> m [[a]]
strings r1 r2 g = Gen.list r1 (Gen.list r2 g)
alphaStringNE :: MonadGen m => m [Char]
alphaStringNE = Gen.list (Range.linear 1 25) Gen.alpha
randSpace :: MonadGen m => m [Char]
randSpace = Gen.list (Range.linear 0 1) (return ' ')
{-
instance (MonadGen m, Semigroup a) => Semigroup (m a) where
  g1 <> g2 =  do 
    a1 <- g1
    a2 <- g2
    return $ a1 <> a2-}