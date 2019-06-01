module Extensions.Monad(module Extensions.Monad, module Control.Monad) where


import qualified Control.Monad.State.Strict as S
import           Control.Monad
import           Extensions.Errors
import           Data.Functor.Identity (Identity(..))

takeJust :: Maybe a -> a
takeJust (Just x) = x;
takeJust _ = errCant

rightsIfAll :: [Either String b] -> Either String [b]
rightsIfAll [] = Right []
rightsIfAll rs = case foldl fldFunc ("", []) rs of
  ("", table) -> Right table
  (str, _) -> Left str
  where
    fldFunc (l, t) (Left str) = (l ++ "\n" ++ str, t)
    fldFunc ("", t) (Right elm) = ("", elm:t)
    fldFunc (l, _) (Right _) = (l, [])

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap fmap fmap

infixl 4 <.>
(<.>) :: (Applicative f) => f (a -> b) -> a -> f b
f <.> a = f <*> pure a


liftT :: Monad m => S.StateT s Identity a -> S.StateT s m a
liftT = S.mapStateT (\(Identity (a, s)) -> return (a, s))

ifdM :: Monad a => a () -> Bool -> a ()
ifdM th b = if b then th else return ()

ifdrM :: Monad a => a () -> Bool -> a ()
ifdrM el = ifdM el . not