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
  , FieldIndex(..)
  , FieldLooks(..)
  , FieldType(..)
  , fieldTypeIncludes
  , parseFieldLooks
  , parseFieldType
  , renderCompatibleEntries
  , renderFieldIndex
  , renderFieldLooks
  , renderFieldType
  ) where

import           Control.DeepSeq.Generics (genericRnf)

import           Data.Ix (Ix)
import qualified Data.Text as T
import           Data.Vector.Unboxed.Deriving (derivingUnbox)

import           GHC.Generics (Generic)

import           P

import           Prelude (fromEnum, toEnum)

import           Warden.Data.Poset

data FieldType =
    TextField
  | BooleanField
  | IntegralField
  | RealField
  deriving (Eq, Show, Enum, Bounded, Ord, Ix, Generic)

instance NFData FieldType where rnf = genericRnf

-- | Truth table for 'FieldType' set inclusion.
--
-- Looks like this:
--
-- Real+---------Text-------+ Boolean
--     |
--     |
--     v
-- Integral
instance Poset FieldType where
  TextField        <=| TextField        = True
  TextField        <=| _                = False

  RealField        <=| TextField        = True
  RealField        <=| RealField        = True
  RealField        <=| _                = False

  IntegralField    <=| TextField        = True
  IntegralField    <=| RealField        = True
  IntegralField    <=| IntegralField    = True
  IntegralField    <=| _                = False

  BooleanField     <=| TextField        = True
  BooleanField     <=| BooleanField     = True
  BooleanField     <=| _                = False

$(derivingUnbox "fieldType"
  [t| FieldType -> Int |]
  [| \x -> fromEnum x |]
  [| \y -> toEnum y |])

renderFieldType :: FieldType -> Text
renderFieldType TextField = "text-field"
renderFieldType BooleanField = "boolean-field"
renderFieldType IntegralField = "integral-field"
renderFieldType RealField = "real-field"

parseFieldType :: Text -> Maybe FieldType
parseFieldType "text-field" = pure TextField
parseFieldType "boolean-field" = pure BooleanField
parseFieldType "integral-field" = pure IntegralField
parseFieldType "real-field" = pure RealField
parseFieldType _ = Nothing

data FieldLooks =
    LooksEmpty
  | LooksIntegral
  | LooksReal
  | LooksText
  | LooksBoolean
  deriving (Eq, Show, Ord, Enum, Bounded, Ix, Generic)

instance NFData FieldLooks where rnf = genericRnf

$(derivingUnbox "FieldLooks"
  [t| FieldLooks -> Int |]
  [| \x -> fromEnum x |]
  [| \y -> toEnum y |])

renderFieldLooks :: FieldLooks -> Text
renderFieldLooks LooksEmpty = "looks-empty"
renderFieldLooks LooksIntegral = "looks-integral"
renderFieldLooks LooksReal = "looks-real"
renderFieldLooks LooksText = "looks-text"
renderFieldLooks LooksBoolean = "looks-boolean"

parseFieldLooks :: Text -> Maybe FieldLooks
parseFieldLooks "looks-empty" = pure LooksEmpty
parseFieldLooks "looks-integral" = pure LooksIntegral
parseFieldLooks "looks-real" = pure LooksReal
parseFieldLooks "looks-text" = pure LooksText
parseFieldLooks "looks-boolean" = pure LooksBoolean
parseFieldLooks _ = Nothing

-- | Schema field types kinda look like a join semilattice under set
-- inclusion. For example, if a given field is classified as a 
-- 'RealField', it's not an anomaly (from the fairly limited
-- perspective of schema inference) if some of its values are classified as
-- 'LooksIntegral' (e.g., from the set {1.3, 2.4, 5}).
fieldTypeIncludes :: FieldType -> FieldLooks -> Bool
fieldTypeIncludes ft fl = case ft of
  TextField -> True
  BooleanField -> fl == LooksBoolean || fl == LooksEmpty
  IntegralField -> fl == LooksIntegral || fl == LooksEmpty
  RealField -> fl == LooksReal || fl == LooksIntegral || fl == LooksEmpty

-- | Number of observed values which are compatible with a given field type.
newtype CompatibleEntries =
  CompatibleEntries {
    unCompatibleEntries :: Int64
  } deriving (Eq, Show, Ord, Num, Generic)

instance NFData CompatibleEntries where rnf = genericRnf

renderCompatibleEntries :: CompatibleEntries -> Text
renderCompatibleEntries (CompatibleEntries n) = T.pack $ show n

$(derivingUnbox "CompatibleEntries"
  [t| CompatibleEntries -> Int64 |]
  [| \(CompatibleEntries x) -> x |]
  [| \x -> (CompatibleEntries x) |])

-- | Starting at zero.
newtype FieldIndex =
  FieldIndex {
    unFieldIndex :: Int
  } deriving (Eq, Show, Ord, Generic)

instance NFData FieldIndex where rnf = genericRnf

$(derivingUnbox "FieldIndex"
  [t| FieldIndex -> Int |]
  [| \(FieldIndex x) -> x |]
  [| \x -> (FieldIndex x) |])

renderFieldIndex :: FieldIndex -> Text
renderFieldIndex = renderIntegral . unFieldIndex
