{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Warden.Debug (
    debugPrintLn
  ) where

import           Data.Text (Text)
import           Data.Text.IO (putStrLn)

import           P

import           System.IO (IO)

import           Warden.Data.Check

debugPrintLn :: Verbosity -> Text -> IO ()
debugPrintLn verb msg =
  when (verb == Verbose) $ putStrLn msg
