{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Warden.Marker(
    readFileMarker
  , readViewMarker
  , writeFileMarker
  , writeViewMarker
  ) where

import           Control.Monad.IO.Class (liftIO)

import           Data.Aeson (encode, decode')
import           Data.Aeson.Types (Value, parseEither)
import           Data.ByteString.Lazy (writeFile, readFile)
import qualified Data.Text as T

import           P

import           System.IO (IO, FilePath)

import           Warden.Data
import           Warden.Error
import           Warden.Serial.Json.Marker

import           X.Control.Monad.Trans.Either (EitherT, firstEitherT, hoistEither, eitherTFromMaybe)

writeFileMarker :: FileMarker -> EitherT WardenError IO ()
writeFileMarker fm =
  let markf = fileToMarker $ fmViewFile fm
      markJson = encode $ fromFileMarker fm in
  liftIO $ writeFile markf markJson

writeViewMarker :: ViewMarker -> EitherT WardenError IO ()
writeViewMarker vm =
  let markf = viewToMarker $ vmView vm
      markJson = encode $ fromViewMarker vm in
  liftIO $ writeFile markf markJson

readJson :: FilePath -> EitherT WardenError IO Value
readJson fp = do
  bs <- liftIO $ readFile fp
  eitherTFromMaybe (WardenMarkerError $ MarkerDecodeError "invalid json") $
    pure $ decode' bs

readFileMarker :: FilePath -> EitherT WardenError IO FileMarker
readFileMarker fp = do
  js <- readJson fp
  firstEitherT (WardenMarkerError . MarkerDecodeError . T.pack) . hoistEither $
    parseEither toFileMarker js

readViewMarker :: FilePath -> EitherT WardenError IO ViewMarker
readViewMarker fp = do
  js <- readJson fp
  firstEitherT (WardenMarkerError . MarkerDecodeError . T.pack) . hoistEither $
    parseEither toViewMarker js
