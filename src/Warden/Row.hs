{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}

module Warden.Row (
    readView
  , readViewChunk
  , readViewFile
  ) where

import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Resource (ResourceT)

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Conduit (Source, Conduit, (=$=), await, yield)
import           Data.Csv ()
import           Data.Csv (DecodeOptions(..), HasHeader(..))
import           Data.Csv (defaultDecodeOptions)
import           Data.Csv.Incremental (Parser(..), decodeWith)
import           Data.List.NonEmpty (NonEmpty)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Vector (Vector)
import           Data.Word (Word8)

import           Data.String (String)

import           P

import           System.IO

import           Warden.Data
import           Warden.Error

import           X.Control.Monad.Trans.Either (EitherT, left)
import           X.Data.Conduit.Binary (slurp, sepByByteBounded)

readView :: Separator
         -> LineBound
         -> NonEmpty ViewFile
         -> Source (EitherT WardenError (ResourceT IO)) Row
readView sep lb vfs =
  sequence_ $ (readViewFile sep lb) <$> vfs

newline :: Word8
newline = 0x0a

readViewFile :: Separator
             -> LineBound
             -> ViewFile
             -> Source (EitherT WardenError (ResourceT IO)) Row
readViewFile (Separator sep) (LineBound lb) vf =
  let fp = viewFilePath vf in
  slurp fp 0 Nothing 
    =$= sepByByteBounded newline lb
    =$= interpLines
    =$= decodeRows vf (decodeWith opts NoHeader)
  where
    opts = defaultDecodeOptions { decDelimiter = sep }

readViewChunk :: Separator
              -> LineBound
              -> ViewFile
              -> Chunk
              -> Source (EitherT WardenError (ResourceT IO)) Row
readViewChunk (Separator sep) (LineBound lb) vf (Chunk offset size) =
  let fp = viewFilePath vf in
  slurp fp (unChunkOffset offset) (Just $ unChunkSize size)
    =$= sepByByteBounded newline lb
    =$= interpLines
    =$= decodeRows vf (decodeWith opts NoHeader)
  where
    opts = defaultDecodeOptions { decDelimiter = sep }

interpLines :: Conduit ByteString (EitherT WardenError (ResourceT IO)) ByteString
interpLines =
  await >>= \case
    Just v' -> yield (BS.snoc v' newline) >> interpLines
    Nothing -> pure ()

decodeRows :: ViewFile -> Parser (Vector Text) -> Conduit ByteString (EitherT WardenError (ResourceT IO)) Row
decodeRows vf (Fail _ e) =
  lift . left . WardenLoadError . RowDecodeFailed vf $ T.pack e
decodeRows vf (Many rs cont) = do
  mapM_ yield $ toRow <$> rs
  more <- await
  maybe (pure ()) (decodeRows vf . cont) more
decodeRows _ (Done rs) =
  mapM_ yield $ toRow <$> rs

toRow :: Either String (Vector Text) -> Row
toRow (Right !r) = SVFields r
toRow (Left !e)  = RowFailure $ T.pack e
{-# INLINE toRow #-}
