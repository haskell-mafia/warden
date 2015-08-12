{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Warden.Fold(
  countFields
) where

import           Control.Monad

import           P

import           Pipes
import qualified Pipes.Prelude              as PP

import           Warden.Data

countFields :: (Monad m)
            => Producer Row m ()
            -> m SVParseState
countFields = PP.fold updateSVParseState initialSVParseState id
