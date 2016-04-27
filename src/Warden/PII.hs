{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

module Warden.PII (
    checkPII
  , combinePIIObservations
  , updatePIIObservations
  ) where

import qualified Data.Attoparsec.ByteString as AB
import           Data.ByteString (ByteString)
import           Data.List.NonEmpty (NonEmpty(..), (<|), nonEmpty)
import qualified Data.List.NonEmpty as NE
import           Data.Semigroup ((<>))

import           P hiding ((<>))

import           Warden.Data.Field
import           Warden.Data.PII
import           Warden.Parser.PII

updatePIIObservations :: MaxPIIObservations
                           -> FieldIndex
                           -> PIIObservations
                           -> ByteString
                           -> PIIObservations
updatePIIObservations mpo fi acc bs = {-# SCC updatePIIObservations #-}
  maybe acc update' $ checkPII bs
  where
    update' pii =
      let o = PotentialPII pii fi in case acc of
        NoPIIObservations ->
          PIIObservations $ pure o
        PIIObservations os ->
          let os' = o <| os in
          if NE.length os' > (unMaxPIIObservations mpo)
            then TooManyPIIObservations
            else PIIObservations $!! os'
        TooManyPIIObservations ->
            TooManyPIIObservations
{-# INLINE updatePIIObservations #-}

combinePIIObservations :: MaxPIIObservations -> PIIObservations -> PIIObservations -> PIIObservations
combinePIIObservations _ NoPIIObservations NoPIIObservations = {-# SCC combinePIIObservations #-}
  NoPIIObservations
combinePIIObservations _ x NoPIIObservations = {-# SCC combinePIIObservations #-}
  x
combinePIIObservations _ NoPIIObservations y = {-# SCC combinePIIObservations #-}
  y
combinePIIObservations _ TooManyPIIObservations _ = {-# SCC combinePIIObservations #-}
  TooManyPIIObservations
combinePIIObservations _ _ TooManyPIIObservations = {-# SCC combinePIIObservations #-}
  TooManyPIIObservations
combinePIIObservations (MaxPIIObservations mpo) (PIIObservations po1) (PIIObservations po2) = {-# SCC combinePIIObservations #-}
  let po' = po1 <> po2 in
  if (NE.length po') > mpo
    then TooManyPIIObservations
    else PIIObservations po'
{-# INLINE combinePIIObservations #-}

checkPII :: ByteString -> Maybe PIIType
checkPII bs = {-# SCC checkPII #-}
  let piis = [phoneNumber, emailAddress] in
  case nonEmpty (catMaybes piis) of
    Nothing -> Nothing
    Just (p:|_) -> pure p
  where
    phoneNumber = case AB.parseOnly phoneNumberP bs of
      Left _ -> Nothing
      Right () -> pure PhoneNumber

    emailAddress = case AB.parseOnly emailP bs of
      Left _ -> Nothing
      Right () -> pure EmailAddress
{-# INLINE checkPII #-}
