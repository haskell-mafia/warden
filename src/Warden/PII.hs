{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

module Warden.PII (
    checkPII
  , updateFieldPIIObservations
  ) where

import qualified Data.Attoparsec.ByteString as AB
import           Data.ByteString (ByteString)
import           Data.List.NonEmpty (NonEmpty(..), (<|), nonEmpty)
import qualified Data.List.NonEmpty as NE

import           P

import           Warden.Data.Field
import           Warden.Data.PII
import           Warden.Parser.PII

updateFieldPIIObservations :: MaxPIIObservations
                           -> FieldIndex
                           -> ByteString
                           -> PIIObservations
                           -> PIIObservations
updateFieldPIIObservations mpo fi bs acc = {-# SCC updateFieldPIIObservations #-}
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
{-# INLINE updateFieldPIIObservations #-}

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
