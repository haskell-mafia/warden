{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}

{- This module will probably live in brandix soon. -}

module Warden.Data.Field (
    CompatibleEntries(..)
  , FieldHistogram(..)
  , FieldLooks(..)
  , FieldType(..)
  , fieldTypeIncludes
  , renderCompatibleEntries
  , renderFieldHistogram
  ) where

import           Data.Ix (Ix)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import           Data.Vector.Unboxed.Deriving (derivingUnbox)

import           GHC.Generics (Generic)

import           P

import           Prelude (fromEnum, toEnum)

data FieldType =
    TextField
  | CategoricalField
  | BooleanField
  | IntegralField
  | RealField
  deriving (Eq, Show, Enum, Bounded, Ord, Ix, Generic)

instance NFData FieldType

$(derivingUnbox "fieldType"
  [t| FieldType -> Int |]
  [| \x -> fromEnum x |]
  [| \y -> toEnum y |])

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

$(derivingUnbox "FieldLooks"
  [t| FieldLooks -> Int |]
  [| \x -> fromEnum x |]
  [| \y -> toEnum y |])

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

-- | Number of observed values which are compatible with a given field type.
newtype CompatibleEntries =
  CompatibleEntries {
    unCompatibleEntries :: Int64
  } deriving (Eq, Show, Ord, Num, Generic)

instance NFData CompatibleEntries

renderCompatibleEntries :: CompatibleEntries -> Text
renderCompatibleEntries (CompatibleEntries n) = T.pack $ show n

$(derivingUnbox "CompatibleEntries"
  [t| CompatibleEntries -> Int64 |]
  [| \(CompatibleEntries x) -> x |]
  [| \x -> (CompatibleEntries x) |])

-- | Map of field types to the number of observed values which seem compatible
-- with that type.
newtype FieldHistogram =
  FieldHistogram {
    unFieldHistogram :: VU.Vector CompatibleEntries
  } deriving (Eq, Show, Generic)

renderFieldHistogram :: FieldHistogram -> Text
renderFieldHistogram (FieldHistogram cs) =
  T.pack . show . V.toList .
    V.zip (V.fromList [minBound..maxBound] :: V.Vector FieldType) .
      V.map renderCompatibleEntries $ VU.convert cs

instance NFData FieldHistogram
