{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

{- This module will probably end up in brandix. -}

module Warden.Data.Inference (
    FieldHistogram(..)
  , FieldMatchThreshold(..)
  , renderFieldHistogram
  , renderFieldMatchThreshold
  ) where

import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

import           GHC.Generics (Generic)

import           P

import           Warden.Data.Field

newtype FieldMatchThreshold =
  FieldMatchThreshold {
    unFieldMatchThreshold :: Double
  } deriving (Eq, Show)

renderFieldMatchThreshold :: FieldMatchThreshold -> Text
renderFieldMatchThreshold = T.pack . show

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
