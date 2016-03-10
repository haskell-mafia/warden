{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{- Most of this module will probably end up in brandix. -}

module Warden.Inference (
    inferSchema
  , validateViewMarkers
  , viewMarkerMismatch
  ) where

import           Control.Lens ((^.))

import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Text (Text)
import qualified Data.Text as T

import           P

import           Warden.Data
import           Warden.Error

viewMarkerMismatch :: ViewMarker -> ViewMarker -> Either Text ()
viewMarkerMismatch a b = do
  validateVersion (vmVersion a) (vmVersion b)
  validateView (vmView a) (vmView b)
  validateTotalFields (fields' a) (fields' b)
  where
    validateVersion = validateEq "vmVersion"

    validateView = validateEq "vmView"

    validateTotalFields = validateEq "numFields"

    validateEq ctx x y =
      if x == y
        then pure ()
        else Left $ T.concat [
                 ctx
               , ": "
               , T.pack (show x)
               , " /= "
               , T.pack (show y)
               ]

    fields' vm' = (vmViewCounts $ vmMetadata vm') ^. numFields

validateViewMarkers :: NonEmpty ViewMarker -> Either InferenceError ()
validateViewMarkers (m:|ms) = go m ms
  where
    go _ [] = pure ()
    go prev (m':ms') = case viewMarkerMismatch prev m' of
      Right () -> go m' ms'
      Left f -> Left $ MarkerValidationFailure f

inferSchema :: NonEmpty ViewMarker -> Either WardenError Schema
inferSchema vms = do
  first WardenInferenceError $ validateViewMarkers vms
  Left WardenNotImplementedError
