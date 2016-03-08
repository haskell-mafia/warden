{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

module Warden.Data.Field (
    FieldLooks(..)
  , FieldType(..)
  ) where

import           Data.Ix (Ix)

import           GHC.Generics (Generic)

import           P

-- FIXME: unknown type
data FieldType =
    TextField
  | CategoricalField
  | BooleanField
  | IntegralField
  | RealField
  deriving (Eq, Show, Enum, Bounded, Ix, Generic)

instance NFData FieldType

data FieldLooks =
    LooksEmpty
  | LooksIntegral
  | LooksReal
  | LooksText
  | LooksCategorical
  | LooksBoolean
  | LooksBroken -- ^ Not valid UTF-8.
  deriving (Eq, Show, Ord, Enum, Bounded, Ix, Generic)

instance NFData FieldLooks

