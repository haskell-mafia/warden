{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Warden.Serial.Json.View(
    fromViewFile
  , toViewFile
  , fromView
  , toView
  ) where

import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.Text as T

import           P

import           Warden.Data.View

fromViewFile :: ViewFile -> Value
fromViewFile (ViewFile f) = String $ T.pack f

toViewFile :: Value -> Parser ViewFile
toViewFile (String s) = pure . ViewFile $ T.unpack s
toViewFile x          = typeMismatch "Warden.Data.View.ViewFile" x

fromView :: View -> Value
fromView (View v) = String $ T.pack v

toView :: Value -> Parser View
toView (String s) = pure . View $ T.unpack s
toView x          = typeMismatch "Warden.Data.View.View" x
