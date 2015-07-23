{-# LANGUAGE NoImplicitPrelude #-}

module Warden.IO (
    readRows
  ) where

import P

import Control.Monad.Trans.Either
import Data.Word
import Pipes
import System.IO

import Warden.Data
import Warden.Error

readRows :: Word8
         -> Producer SVRow (EitherT WardenError IO) ()
readRows = fail "nyi"


