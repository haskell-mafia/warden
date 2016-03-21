{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Warden.Data.FieldAnomaly (
    AnomalousField(..)
  , FieldAnomaly(..)
  , checkFieldType
  , fieldAnomalies
  , formAnomalies
  ) where

import           Data.List.NonEmpty (NonEmpty, nonEmpty)
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

import           P

import           Warden.Data.Field
import           Warden.Data.Row
import           Warden.Data.Schema
import           Warden.Data.TextCounts

data FieldAnomaly =
    FieldAnomaly !FieldLooks !ObservationCount
  deriving (Eq, Show)

data AnomalousField =
    AnomalousField !FieldIndex !FieldType !(NonEmpty FieldAnomaly)
  | AnomalousForm !FieldIndex !FieldUniques !UniqueTextCount
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


formAnomalies :: FieldForm -> UniqueTextCount -> FieldIndex -> Maybe AnomalousField
-- It's not considered an error if a freeform field occasionally looks
-- categorical.
formAnomalies FreeForm _ _ =
  Nothing
formAnomalies (CategoricalForm expected) LooksFreeform idx =
  Just $ AnomalousForm idx expected LooksFreeform
formAnomalies (CategoricalForm expected) (UniqueTextCount tc) idx =
  if (S.size tc) > (unFieldUniques expected)
    then Just $ AnomalousForm idx expected (UniqueTextCount tc)
    else Nothing
