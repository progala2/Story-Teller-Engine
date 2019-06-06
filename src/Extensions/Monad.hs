-- |
-- Module      : Monad
-- Description : Helpers for Monad usage.
module Extensions.Monad(module Extensions.Monad, module Control.Monad) where

import           Control.Monad
import           Extensions.Errors

-- | Take Just value or throw an error.
takeJust :: Maybe a -> a
takeJust (Just x) = x;
takeJust _ = errCant

-- | Convert table of Eithers to one Either with table of values. 
-- 
-- The result will be Left if there is at least one Left in the initial table.
-- String from the Left value will provide all the Lefts values found during mapping.
rightsIfAll :: [Either String b] -> Either String [b]
rightsIfAll [] = Right []
rightsIfAll rs = case foldl fldFunc ("", []) rs of
  ("", table) -> Right table
  (str, _) -> Left str
  where
    fldFunc (l, t) (Left str) = (l ++ "\n" ++ str, t)
    fldFunc ("", t) (Right elm) = ("", elm:t)
    fldFunc (l, _) (Right _) = (l, [])

-- | It is the same as writting:
-- 
-- @ 
--  fmap fmap fmap
-- or
--  (f <$>) <$> g
-- @
infixl 4 <$$>
(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap fmap fmap

-- | It is a helper to avoid writing:
--
-- >>> f <*> pure a
infixl 4 <.>
(<.>) :: (Applicative f) => f (a -> b) -> a -> f b
f <.> a = f <*> pure a

-- | Returns @a ()@ if 'True', or @return ()@ if 'False'.
ifdM :: Monad a => a () -> Bool -> a ()
ifdM th b = if b then th else return ()
-- | Returns @a ()@ if 'False', or @return ()@ if 'True'.
ifdrM :: Monad a => a () -> Bool -> a ()
ifdrM el = ifdM el . not