{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Warden.Data.FieldAnomaly (
    AnomalousField(..)
  , FieldAnomaly(..)
  , checkFieldType
  , fieldAnomalies
  ) where

import           Data.List.NonEmpty (NonEmpty, nonEmpty)
import           Data.Array (Array)
import qualified Data.Array as A

import           P

import           Warden.Data.Field
import           Warden.Data.Row

data FieldAnomaly =
    FieldAnomaly !FieldLooks !ObservationCount
  deriving (Eq, Show)

data AnomalousField =
    AnomalousField !FieldType !(NonEmpty FieldAnomaly)
  deriving (Eq, Show)

-- | For a given field type and set of value observations of that field, find
-- any which look anomalous.
fieldAnomalies :: FieldType -> Array FieldLooks ObservationCount -> Maybe AnomalousField
fieldAnomalies ft obs = do
  as <- nonEmpty . catMaybes . fmap (uncurry (checkFieldType ft)) $ A.assocs obs
  pure $ AnomalousField ft as

checkFieldType :: FieldType -> FieldLooks -> ObservationCount -> Maybe FieldAnomaly
checkFieldType ft looks cnt
  | fieldTypeIncludes ft looks = Nothing
  | otherwise = if cnt > (ObservationCount 0)
                  then Just $ FieldAnomaly looks cnt
                  else Nothing
