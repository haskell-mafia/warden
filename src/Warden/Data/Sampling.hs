{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Warden.Data.Sampling(
    Sample(..)
  ) where

import           Control.DeepSeq.Generics (genericRnf)

import qualified Data.Vector.Unboxed as VU

import           GHC.Generics (Generic)

import           P

data Sample =
    NoSample
  | Sample !(VU.Vector Double)
  deriving (Eq, Show, Generic)

instance NFData Sample where rnf = genericRnf
