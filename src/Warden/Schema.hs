{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Warden.Schema(
      readSchema
    , renderSchema
    , writeSchema
  ) where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Resource (ResourceT)

import           Data.Aeson (eitherDecode')
import           Data.Aeson.Encode.Pretty (encodePretty)
import           Data.Aeson.Types (parseEither)
import qualified Data.ByteString.Lazy as BSL
import           Data.ByteString.Lazy (readFile, writeFile)
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8)

import           P

import           System.IO (IO)

import           Warden.Data.Schema
import           Warden.Error
import           Warden.Serial.Json

import           X.Control.Monad.Trans.Either (EitherT, firstEitherT)
import           X.Control.Monad.Trans.Either (hoistEither)

readSchema :: SchemaFile -> EitherT WardenError (ResourceT IO) Schema
readSchema sf@(SchemaFile fp) = do
  bs <- liftIO $ readFile fp
  json <- firstEitherT err . hoistEither $ eitherDecode' bs
  firstEitherT err . hoistEither $ parseEither toSchema json
  where
    err = WardenSchemaError . SchemaDecodeError sf . T.pack

renderSchema :: Schema -> Text
renderSchema = decodeUtf8 . BSL.toStrict . renderSchema'

renderSchema' :: Schema -> BSL.ByteString
renderSchema' = encodePretty . fromSchema

writeSchema :: Schema -> SchemaFile -> EitherT WardenError (ResourceT IO) ()
writeSchema s (SchemaFile fp) =
  liftIO . writeFile fp $ renderSchema' s
