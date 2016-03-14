{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- This module will probably end up in brandix. -}

module Warden.Data.Inference (
    FieldHistogram(..)
  , FieldMatchRatio(..)
  , NormalizedEntries(..)
  , renderFieldHistogram
  , renderFieldMatchRatio
  ) where

import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import           Data.Vector.Unboxed.Deriving (derivingUnbox)

import           GHC.Generics (Generic)

import           P

import           Warden.Data.Field

-- | Ratio of compatible observations with the total number of rows.
newtype FieldMatchRatio =
  FieldMatchRatio {
    unFieldMatchRatio :: Double
  } deriving (Eq, Show)

renderFieldMatchRatio :: FieldMatchRatio -> Text
renderFieldMatchRatio = T.pack . show . unFieldMatchRatio

-- | Map of field types to the number of observed values which seem compatible
-- with that type.
newtype FieldHistogram =
  FieldHistogram {
    unFieldHistogram :: VU.Vector CompatibleEntries
  } deriving (Eq, Show, Generic)

instance NFData FieldHistogram

renderFieldHistogram :: FieldHistogram -> Text
renderFieldHistogram (FieldHistogram cs) =
  T.intercalate ", " . fmap (\(t,n) -> (T.pack $ show t <> " = " <> show n)) .
    V.toList . V.zip (V.fromList [minBound..maxBound] :: V.Vector FieldType) .
      V.map renderCompatibleEntries $ VU.convert cs

-- | Ratio of 'CompatibleEntries' to total 'RowCount'.
newtype NormalizedEntries =
  NormalizedEntries {
    unNormalizedEntries :: Double
  } deriving (Eq, Show, Generic, Ord, Num, Fractional)

$(derivingUnbox "NormalizedEntries"
  [t| NormalizedEntries -> Double |]
  [| \(NormalizedEntries x) -> x  |]
  [| \x -> (NormalizedEntries x)  |])

instance NFData NormalizedEntries
