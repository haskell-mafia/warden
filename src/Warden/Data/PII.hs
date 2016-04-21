{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

module Warden.Data.PII (
    MaxPIIObservations(..)
  , PIIType(..)
  , PotentialPII(..)
  , PIIObservations(..)
  ) where

import           Data.List.NonEmpty (NonEmpty)

import           GHC.Generics (Generic)

import           P

import           Warden.Data.Field

-- | May want to add 'Name' at some point if worth the effort.
data PIIType =
    EmailAddress
  | PhoneNumber
  | Address
  | DateOfBirth
  deriving (Eq, Show, Generic, Enum, Bounded)

instance NFData PIIType

data PotentialPII =
    PotentialPII !PIIType {-# UNPACK #-} !FieldIndex
  deriving (Eq, Show, Generic)

instance NFData PotentialPII

newtype MaxPIIObservations =
  MaxPIIObservations {
    unMaxPIIObservations :: Int
  } deriving (Eq, Show, Generic)

instance NFData MaxPIIObservations

data PIIObservations =
    NoPIIObservations
  | PIIObservations !(NonEmpty PotentialPII)
  | TooManyPIIObservations
  deriving (Eq, Show, Generic)

instance NFData PIIObservations
