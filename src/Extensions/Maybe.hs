module Extensions.Maybe where

import Extensions.Errors

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
f <$$> fga = (f <$>) <$> fga 