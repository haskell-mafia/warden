{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-
Implementation of Vitter's Algorithm Z for constant-memory single-pass
uniform sampling.

Jeffrey S. Vitter. 1985. Random sampling with a reservoir.
ACM Trans. Math. Softw. 11, 1 (March 1985), 37-57.
DOI=http://dx.doi.org/10.1145/3147.3165
-}
module Warden.Sampling.Reservoir (
    ReservoirAcc(..)
  , Seen(..)
  , ReservoirSize(..)
  , Sigma(..)
  , X(..)
  ) where

import           Data.Vector.Unboxed (Vector)

data ReservoirAcc =
  ReservoirAcc {
    reservoir      :: Vector Double
  , reservoirDelta :: Int -- Records to skip before sampling again.
  , reservoirT     :: Int -- Records we've seen so far.
  } deriving (Eq, Show)

newtype ReservoirSize =
  ReservoirSize {
    unReservoirSize :: Int
  } deriving (Eq, Show, Num)

newtype Seen =
  Seen {
    unSeen :: Int
  } deriving (Eq, Show, Num)

-- | Discrete random variable parameterised by the size of the sample and the
-- number of records seen thus far. Sigma(n, t) is the number of records skipped
-- over for a sample of size n, at the index t.
--
-- PMF: \( f(s) = \frac{n}{t+s+1} \frac{t^{\underbar{n}}}{(t+s)^{\underbar{n}}} \)
-- CDF: \( F(s) = 1 - \frac{t^{\underbar{n}}}{(t+s+1)^{\underbar{n}}} \)
data Sigma = Sigma ReservoirSize Seen
  deriving (Show)

-- | Continuous random variable with a distribution approximating F(s),
-- used to generate 'Sigma' without evaluating all those pesky falling powers.
--
-- PDF: \( g(x) = \frac{n}{t+x} (\frac{t}{t+x})^n, x \ge 0 \)
-- CDF: \( G(x) = 1 - (\frac{t}{t+x})^n \) (\( \int_0^x g(x) dx \))
data X = X ReservoirSize Seen
  deriving (Show)
