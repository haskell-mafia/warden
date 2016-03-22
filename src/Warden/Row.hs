{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Warden.Row (
    decodeByteString
  , readView
  , readView'
  , readViewChunk
  , readViewChunk'
  , readViewFile
  ) where

import           Control.Monad.Trans.Resource (ResourceT)

import qualified Data.Attoparsec.ByteString as AB
import           Data.ByteString (ByteString)
import           Data.Char (ord)
import           Data.Conduit (Source, Conduit, (=$=), awaitForever, yield)
import qualified Data.Conduit.List as DC
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Vector (Vector)
import qualified Data.Vector as V

import           Data.String (String)

import           P

import           System.IO

import           Warden.Data
import           Warden.Error
import           Warden.Row.Parser

import           X.Data.Conduit.Binary (slurp, sepByByteBounded)
import           X.Control.Monad.Trans.Either (EitherT)

decodeByteString :: Separator
           -> LineBound
           -> ViewFile
           -> Conduit ByteString (EitherT WardenError (ResourceT IO)) Row
decodeByteString sep (LineBound lb) _vf =
      sepByByteBounded (fromIntegral $ ord '\n') lb
  =$= decodeByteString'
  =$= DC.map toRow
  where
    decodeByteString' = awaitForever $ \l ->
      yield . second unRawRecord $ AB.parseOnly (rawRecordP sep) l
{-# INLINE decodeByteString #-}

decodeRecord :: Separator
             -> LineBound
             -> ViewFile
             -> Conduit ByteString (EitherT WardenError (ResourceT IO)) Row
decodeRecord = decodeByteString
{-# INLINE decodeRecord #-}

readView :: Separator
         -> LineBound
         -> NonEmpty ViewFile
         -> Source (EitherT WardenError (ResourceT IO)) Row
readView sep lb vfs =
  readView' (decodeRecord sep lb) vfs

readView' :: (ViewFile -> Conduit ByteString (EitherT WardenError (ResourceT IO)) Row)
          -> NonEmpty ViewFile
          -> Source (EitherT WardenError (ResourceT IO)) Row
readView' c vfs =
  sequence_ $ (readViewFile' c) <$> vfs

readViewFile :: Separator
             -> LineBound
             -> ViewFile
             -> Source (EitherT WardenError (ResourceT IO)) Row
readViewFile sep lb = readViewFile' (decodeRecord sep lb)

readViewFile' :: (ViewFile -> Conduit ByteString (EitherT WardenError (ResourceT IO)) Row)
              -> ViewFile
              -> Source (EitherT WardenError (ResourceT IO)) Row
readViewFile' c vf =
  let fp = viewFilePath vf in
  slurp fp 0 Nothing 
    =$= c vf

readViewChunk :: Separator
              -> LineBound
              -> ViewFile
              -> Chunk
              -> Source (EitherT WardenError (ResourceT IO)) Row
readViewChunk sep lb =
  readViewChunk' (decodeRecord sep lb)

readViewChunk' :: (ViewFile -> Conduit ByteString (EitherT WardenError (ResourceT IO)) Row)
               -> ViewFile
               -> Chunk
               -> Source (EitherT WardenError (ResourceT IO)) Row
readViewChunk' c vf (Chunk offset size) =
  let fp = viewFilePath vf in
  slurp fp (unChunkOffset offset) (Just $ unChunkSize size)
    =$= c vf

-- FIXME: slow
toRow :: Either String (Vector ByteString) -> Row
toRow (Right !r) =
  let ts = V.map T.decodeUtf8' r
      (bads, goods) = partitionEithers $ V.toList ts in
  if null bads
    then
      SVFields $ V.fromList goods
    else
      RowFailure $ T.concat [
          "could not decode fields: "
        , T.intercalate ", " (fmap (T.pack . show) $ bads)
        ]
toRow (Left !e) =
  RowFailure $ T.pack e
{-# INLINE toRow #-}
