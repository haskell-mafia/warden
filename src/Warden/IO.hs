{-# LANGUAGE NoImplicitPrelude #-}

module Warden.IO (
    readSVRows
  ) where

import           P

import           Control.Monad.Trans.Either
import           Data.Csv                   (DecodeOptions (..),
                                             defaultDecodeOptions)
import           Data.Csv.Streaming
import qualified Data.Text                  as T
import           Pipes
import qualified Pipes.ByteString           as PB
import           System.IO

import           Warden.Data
import           Warden.Error

-- FIXME(sio): this is going to explode (SVSeparator doesn't do what I
-- thought it did) - this should just take a Producer
readSVRows :: Separator
           -> Handle
           -> Producer Row (EitherT WardenError IO) ()
readSVRows (Separator sep) h = do
    b <- PB.toLazyM $ PB.fromHandle h
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
