{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE BangPatterns #-}

module Warden.Rows (
    readSVHandle
  , readSVView
  , readView
  , readViewFile
  ) where


import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Resource (ResourceT)

import           Data.ByteString (ByteString)
import           Data.Conduit (Source, Conduit, (=$=), await, yield)
import           Data.Csv ()
import           Data.Csv (DecodeOptions(..), HasHeader(..))
import           Data.Csv (defaultDecodeOptions)
import           Data.Csv.Incremental (Parser(..), decodeWith)
import qualified Data.List.NonEmpty as NE
import           Data.List.NonEmpty (NonEmpty)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Vector (Vector)

import           P

import           Pipes (Producer, (>->))
import qualified Pipes.ByteString as PB
import qualified Pipes.Csv as PC
import qualified Pipes.Prelude as PP

import           System.IO

import           Warden.Data
import           Warden.Error

import           X.Control.Monad.Trans.Either (EitherT, left)
import           X.Data.Conduit.Binary (slurp)

readSVView :: Separator
           -> NonEmpty ViewFile
           -> Producer Row (EitherT WardenError IO) ()
readSVView sep fs = do
  -- FIXME: probably want to chunk this
  hs <- liftIO $ mapM (flip openFile ReadMode) (unViewFile <$> (NE.toList fs))
  sequence_ $ (readSVHandle sep) <$> hs

readView :: Separator
         -> LineBound
         -> NonEmpty ViewFile
         -> Source (EitherT WardenError (ResourceT IO)) Row
readView sep lb vfs =
  sequence_ $ read' <$> vfs
  where
    read' = readViewFile sep lb

-- FIXME: actually enforce LineBound
readViewFile :: Separator
             -> LineBound
             -> ViewFile
             -> Source (EitherT WardenError (ResourceT IO)) Row
readViewFile (Separator sep) (LineBound _lb) (ViewFile fp) =
  slurp fp 0 Nothing =$= decodeRows (decodeWith opts NoHeader)
  where
    decodeRows :: Parser (Vector Text) -> Conduit ByteString (EitherT WardenError (ResourceT IO)) Row
    decodeRows (Fail _ e) =
      lift . left . WardenLoadError . RowDecodeFailed $ T.pack e
    decodeRows (Many rs cont) = do
      mapM_ yield $ toRow <$> rs
      more <- await
      maybe (pure ()) (decodeRows . cont) more
    decodeRows (Done rs) =
      mapM_ yield $ toRow <$> rs

    opts = defaultDecodeOptions { decDelimiter = sep }

    toRow (Right !r) = SVFields r
    toRow (Left !e)  = RowFailure $ T.pack e

readSVHandle :: Separator
             -> Handle
             -> Producer Row (EitherT WardenError IO) ()
readSVHandle (Separator sep) h =
  PC.decodeWith opts NoHeader (PB.fromHandle h) >-> PP.map toRow
  where
    opts = defaultDecodeOptions { decDelimiter = sep }

    toRow (Right !r) = SVFields r
    toRow (Left !e)  = RowFailure $ T.pack e
