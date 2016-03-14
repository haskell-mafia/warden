{-# LANGUAGE NoImplicitPrelude #-}

module Warden.Data.Poset(
    Poset
  , (<=|)
  , minima
  ) where

import           Data.Set (Set, (\\))
import qualified Data.Set as S

import           P

-- | Partially-ordered set class.
--
-- Laws:
--
-- * @a <=| a@
-- * @a <=| b && b <=| a ==> a == b@
-- * @a <=| b <=| c ==> a <=| b@
class Eq a => Poset a where
  (<=|) :: a -> a -> Bool
infix 4 <=|

-- | Slow, but don't need this to be fast.
--
-- It's a bit silly to require a poset to have an Ord instance; can switch to
-- Data.HashSet if this becomes an issue.
minima :: (Ord a, Poset a) => Set a -> Set a
minima xs = S.filter nothingGreater xs
  where
    nothingGreater x =
      all' (not . flip (<=|) x) $ xs \\ (S.singleton x)

    all' f s =
      if S.null s
        then True
        else S.map f s == S.singleton True
