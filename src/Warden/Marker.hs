{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Warden.Marker(
    fileMarkerExists
  , readFileMarker
  , readViewMarker
  , viewMarkerExists
  , writeFileMarker
  , writeViewMarker
  , utcNow
  ) where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Resource (ResourceT)

import           Data.Aeson.Encode.Pretty (encodePretty)
import           Data.Aeson (decode')
import           Data.Aeson.Types (Value, parseEither)
import           Data.ByteString.Lazy (writeFile, readFile)
import qualified Data.Text as T
import           Data.Time.Zones (utcTZ)

import           Delorean.Local.DateTime (DateTime, local)

import           P

import           System.Directory (doesFileExist, createDirectoryIfMissing)
import           System.FilePath (takeDirectory)
import           System.IO (IO, FilePath)

import           Warden.Data
import           Warden.Error
import           Warden.Serial.Json.Marker

import           X.Control.Monad.Trans.Either (EitherT, firstEitherT, hoistEither, eitherTFromMaybe)

writeFileMarker :: FileMarker -> EitherT WardenError (ResourceT IO) ()
writeFileMarker fm =
  let markf = fileToMarker $ fmViewFile fm
      markJson = encodePretty $ fromFileMarker fm in
  liftIO $ writeFile markf markJson

writeViewMarker :: ViewMarker -> EitherT WardenError (ResourceT IO) ()
writeViewMarker vm =
  let markf = viewMarkerPath vm
      markd = takeDirectory markf
      markJson = encodePretty $ fromViewMarker vm in liftIO $ do
  createDirectoryIfMissing True markd
  writeFile markf markJson

readJson :: FilePath -> EitherT WardenError (ResourceT IO) Value
readJson fp = do
  bs <- liftIO $ readFile fp
  eitherTFromMaybe (WardenMarkerError $ MarkerDecodeError fp "invalid json") $
    pure $ decode' bs

readFileMarker :: ViewFile -> EitherT WardenError (ResourceT IO) FileMarker
readFileMarker = readFileMarker' . fileToMarker

readFileMarker' :: FilePath -> EitherT WardenError (ResourceT IO) FileMarker
readFileMarker' fp = do
  js <- readJson fp
  firstEitherT (WardenMarkerError . MarkerDecodeError fp . T.pack) . hoistEither $
    parseEither toFileMarker js

readViewMarker :: FilePath -> EitherT WardenError (ResourceT IO) ViewMarker
readViewMarker fp = do
  js <- readJson fp
  firstEitherT (WardenMarkerError . MarkerDecodeError fp . T.pack) . hoistEither $
    parseEither toViewMarker js

viewMarkerExists :: ViewMarker -> IO Bool
viewMarkerExists =
  doesFileExist . viewMarkerPath

fileMarkerExists :: ViewFile -> IO Bool
fileMarkerExists =
  doesFileExist . fileToMarker

utcNow :: IO DateTime
utcNow = local utcTZ
