{-# LANGUAGE NoImplicitPrelude #-}

{- This module will probably end up in brandix. -}

module Warden.Inference (
    inferSchema
  ) where

import          Data.List.NonEmpty (NonEmpty)

import          P

import          Warden.Data
import          Warden.Error

inferSchema :: NonEmpty ViewMarker -> Either WardenError Schema
inferSchema _vms = Left WardenNotImplementedError
