{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Warden.Data.Sampling(
    Sample(..)
  ) where

import qualified Data.Vector.Unboxed as VU

import           GHC.Generics (Generic)

import           P

newtype Sample =
  Sample {
    unSample :: VU.Vector Double
  } deriving (Eq, Show, Generic)

instance NFData Sample
