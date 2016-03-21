{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}

module Warden.Row (
    decodeCassava
  , decodeText
  , readView
  , readView'
  , readViewChunk
  , readViewChunk'
  , readViewFile
  ) where

import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Resource (ResourceT)

import qualified Data.Attoparsec.Text as AT
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Conduit (Source, Conduit, (=$=), await, awaitForever, yield)
import           Data.Conduit.Text (linesBounded, decodeUtf8)
import           Data.Csv ()
import           Data.Csv (DecodeOptions(..), HasHeader(..))
import           Data.Csv (defaultDecodeOptions)
import           Data.Csv.Incremental (Parser(..), decodeWith)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.Text as T
import           Data.Vector (Vector)
import           Data.Word (Word8)

import           Data.String (String)

import           P

import           System.IO

import           Warden.Data
import           Warden.Error
import           Warden.Row.Parser

import           X.Control.Monad.Trans.Either (EitherT, left)
import           X.Data.Conduit.Binary (slurp, sepByByteBounded)

decodeCassava :: Separator
              -> LineBound
              -> ViewFile
              -> Conduit ByteString (EitherT WardenError (ResourceT IO)) Row
decodeCassava (Separator sep) (LineBound lb) vf =
      sepByByteBounded newline lb
  =$= interpLines
  =$= decodeRows vf (decodeWith opts NoHeader)
  where
    opts = defaultDecodeOptions { decDelimiter = sep }
{-# INLINE decodeCassava #-}

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

newline :: Word8
newline = 0x0a

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

interpLines :: Conduit ByteString (EitherT WardenError (ResourceT IO)) ByteString
interpLines =
  awaitForever $ \v -> yield (BS.snoc v newline)
{-# INLINE interpLines #-}

decodeRows :: ViewFile -> Parser (Vector Text) -> Conduit ByteString (EitherT WardenError (ResourceT IO)) Row
decodeRows vf (Fail _ e) =
  lift . left . WardenLoadError . RowDecodeFailed vf $ T.pack e
decodeRows vf (Many rs cont) = do
  mapM_ yield $ toRow <$> rs
  more <- await
  maybe (pure ()) (decodeRows vf . cont) more
decodeRows _ (Done rs) =
  mapM_ yield $ toRow <$> rs
{-# INLINE decodeRows #-}

toRow :: Either String (Vector Text) -> Row
toRow (Right !r) = SVFields r
toRow (Left !e)  = RowFailure $ T.pack e
{-# INLINE toRow #-}
