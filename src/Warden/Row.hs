{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}

module Warden.Row (
    decodeText
  , readView
  , readView'
  , readViewChunk
  , readViewChunk'
  , readViewFile
  ) where

import           Control.Monad.Trans.Resource (ResourceT)

import qualified Data.Attoparsec.Text as AT
import           Data.ByteString (ByteString)
import           Data.Conduit (Source, Conduit, (=$=), awaitForever, yield)
import           Data.Conduit.Text (linesBounded, decodeUtf8)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.Text as T
import           Data.Vector (Vector)

import           Data.String (String)

import           P

import           System.IO

import           Warden.Data
import           Warden.Error
import           Warden.Row.Parser

import           X.Data.Conduit.Binary (slurp)
import           X.Control.Monad.Trans.Either (EitherT)

decodeText :: Separator
           -> LineBound
           -> ViewFile
           -> Conduit ByteString (EitherT WardenError (ResourceT IO)) Row
decodeText sep (LineBound lb) _vf =
      decodeUtf8
  =$= linesBounded lb
  =$= decodeText'
  where
    decodeText' = awaitForever $ \l ->
      yield . toRow . second unRawRecord $ AT.parseOnly (rawRecordP sep) l
{-# INLINE decodeText #-}

decodeRecord :: Separator
             -> LineBound
             -> ViewFile
             -> Conduit ByteString (EitherT WardenError (ResourceT IO)) Row
decodeRecord = decodeText
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

toRow :: Either String (Vector Text) -> Row
toRow (Right !r) = SVFields r
toRow (Left !e)  = RowFailure $ T.pack e
{-# INLINE toRow #-}
