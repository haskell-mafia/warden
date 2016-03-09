{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

{- This module will probably live in brandix soon. -}

module Warden.Data.Field (
    FieldLooks(..)
  , FieldType(..)
  , fieldTypeIncludes
  ) where

import           Data.Ix (Ix)

import           GHC.Generics (Generic)

import           P

data FieldType =
    TextField
  | CategoricalField
  | BooleanField
  | IntegralField
  | RealField
  deriving (Eq, Show, Enum, Bounded, Ord, Ix, Generic)

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

-- | Schema field types kinda look like a join semilattice under set
-- inclusion. For example, if a given field is classified as a 
-- 'CategoricalField', it's not an anomaly (from the fairly limited
-- perspective of schema inference) if some of its values are classified as
-- 'LooksBoolean' (e.g., from the set {"true", "false",
-- "undetermined"}). Likewise e.g., 'LooksIntegral' $$ \subseteq $$
-- 'RealField'.
fieldTypeIncludes :: FieldType -> FieldLooks -> Bool
fieldTypeIncludes ft fl = case ft of
  TextField -> fl /= LooksBroken
  CategoricalField -> fl == LooksCategorical || fl == LooksBoolean || fl == LooksEmpty
  BooleanField -> fl == LooksBoolean || fl == LooksEmpty
  IntegralField -> fl == LooksIntegral || fl == LooksEmpty
  RealField -> fl == LooksReal || fl == LooksIntegral || fl == LooksEmpty
