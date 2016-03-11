{-# LANGUAGE NoImplicitPrelude #-}

module Warden.Data.Poset(
    Poset
  , (<=|)
  , minima
  ) where

import           Data.List ((\\))

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
minima :: Poset a => [a] -> [a]
minima xs = filter nothingGreater xs
  where
    nothingGreater x =
      all (not . flip (<=|) x) $ xs \\ [x]
