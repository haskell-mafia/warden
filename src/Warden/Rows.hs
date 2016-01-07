{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE BangPatterns #-}

module Warden.Rows (
    readSVHandle
  , readSVView
  ) where

import           Data.Csv (DecodeOptions (..))
import qualified Data.List.NonEmpty as NE
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.Text as T

import           P

import           Pipes
import qualified Pipes.ByteString as PB
import           Pipes.Csv
import qualified Pipes.Prelude as PP

import           System.IO

import           Warden.Data
import           Warden.Error

import           X.Control.Monad.Trans.Either

readSVView :: Separator
           -> NonEmpty ViewFile
           -> Producer Row (EitherT WardenError IO) ()
readSVView sep fs = do
  -- FIXME: probably want to chunk this
  hs <- liftIO $ mapM (flip openFile ReadMode) (unViewFile <$> (NE.toList fs))
  sequence_ $ (readSVHandle sep) <$> hs

readSVHandle :: Separator
             -> Handle
             -> Producer Row (EitherT WardenError IO) ()
readSVHandle (Separator sep) h =
  decodeWith opts NoHeader (PB.fromHandle h) >-> PP.map toRow
  where
    opts = defaultDecodeOptions { decDelimiter = sep }

    toRow (Right !r) = SVFields r
    toRow (Left !e)  = RowFailure $ T.pack e

    
