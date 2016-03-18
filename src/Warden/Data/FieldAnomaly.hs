{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Warden.Data.FieldAnomaly (
    AnomalousField(..)
  , FieldAnomaly(..)
  , checkFieldType
  , fieldAnomalies
  ) where

import           Data.List.NonEmpty (NonEmpty, nonEmpty)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

import           P

import           Warden.Data.Field
import           Warden.Data.Row

data FieldAnomaly =
    FieldAnomaly !FieldLooks !ObservationCount
  deriving (Eq, Show)

data AnomalousField =
    AnomalousField !FieldIndex !FieldType !(NonEmpty FieldAnomaly)
  deriving (Eq, Show)

-- | For a given field type and set of value observations of that field, find
-- any which look anomalous.
fieldAnomalies :: FieldType -> VU.Vector ObservationCount -> FieldIndex -> Maybe AnomalousField
fieldAnomalies ft obs idx = do
  as <- nonEmpty . catMaybes . V.toList . V.map (uncurry (checkFieldType ft)) $
    V.zip (V.fromList [minBound..maxBound]) $ VU.convert obs
  pure $ AnomalousField idx ft as

checkFieldType :: FieldType -> FieldLooks -> ObservationCount -> Maybe FieldAnomaly
checkFieldType ft looks cnt
  | fieldTypeIncludes ft looks = Nothing
  | otherwise = if cnt > (ObservationCount 0)
                  then Just $ FieldAnomaly looks cnt
                  else Nothing
