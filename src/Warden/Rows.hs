{-# LANGUAGE NoImplicitPrelude #-}

module Warden.Rows (
    readSVHandle
  , readSVView
  , decodeSVRows
  ) where

import           Data.ByteString.Lazy (ByteString)
import           Data.Csv (DecodeOptions (..), defaultDecodeOptions)
import           Data.Csv.Streaming
import qualified Data.List.NonEmpty as NE
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.Text as T

import           P

import           Pipes
import qualified Pipes.ByteString as PB

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
readSVHandle sep h =
  decodeSVRows sep =<< (PB.toLazyM $ PB.fromHandle h)

decodeSVRows :: Separator
             -> ByteString
             -> Producer Row (EitherT WardenError IO) ()
decodeSVRows (Separator sep) b = do
    yieldRows $ decodeWith opts NoHeader b
  where
    yieldRows (Cons (Right r) rs) = do
        yield $ SVFields r
        yieldRows rs
    yieldRows (Cons (Left rf) rs) = do
        yield $ RowFailure (T.pack rf)
        yieldRows rs
    yieldRows (Nil Nothing _) =
        yield SVEOF
    yieldRows (Nil (Just err) rest) =
        lift . left . LoadError . T.pack $ show err <> "with remaining: " <> show rest

    opts = defaultDecodeOptions { decDelimiter = sep }
