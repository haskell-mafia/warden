{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

module Warden.Data.PII (
    MaxPIIObservations(..)
  , PIIType(..)
  , PotentialPII(..)
  , PIIObservations(..)
  , renderPIIType
  , renderPotentialPII
  ) where

import           Data.AEq (AEq, (===), (~==))
import           Data.List.NonEmpty (NonEmpty, sort)
import qualified Data.Text as T

import           GHC.Generics (Generic)

import           P

import           Warden.Data.Field

-- | May want to add 'Name' at some point if worth the effort.
data PIIType =
    EmailAddress
  | PhoneNumber
  | Address
  | DateOfBirth
  deriving (Eq, Show, Ord, Generic, Enum, Bounded)

instance NFData PIIType

renderPIIType :: PIIType -> Text
renderPIIType EmailAddress = "email address"
renderPIIType PhoneNumber = "phone number"
renderPIIType Address = "address"
renderPIIType DateOfBirth = "date of birth"

data PotentialPII =
    PotentialPII !PIIType {-# UNPACK #-} !FieldIndex
  deriving (Eq, Show, Ord, Generic)

instance NFData PotentialPII

renderPotentialPII :: PotentialPII -> Text
renderPotentialPII (PotentialPII typ ix) = T.concat [
    renderPIIType typ
  , " (field "
  , renderFieldIndex ix
  , ")"
  ]

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

instance AEq PIIObservations where
  (===) = (==)

  NoPIIObservations ~== NoPIIObservations = True
  NoPIIObservations ~== _ = False
  _ ~== NoPIIObservations = False
  TooManyPIIObservations ~== TooManyPIIObservations = True
  TooManyPIIObservations ~== _ = False
  _ ~== TooManyPIIObservations = False
  (PIIObservations xs) ~== (PIIObservations ys) = sort xs == sort ys
