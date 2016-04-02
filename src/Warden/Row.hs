{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Warden.Row (
    combineSVParseState
  , decodeByteString
  , fieldP
  , parseField
  , readView
  , readView'
  , readViewChunk
  , readViewChunk'
  , readViewFile
  , resolveSVParseState
  , updateFieldLooks
  , updateFieldNumericState
  , updateFields
  , updateNumFields
  , updateTextCounts
  , updateSVParseState
  ) where

import           Control.Lens ((%~), (^.))
import           Control.Monad.Trans.Resource (ResourceT)

import qualified Data.Attoparsec.ByteString as AB
import           Data.Bits ((.|.))
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Char (ord)
import           Data.Conduit (Source, Conduit, (=$=), awaitForever, yield)
import qualified Data.Conduit.List as DC
import           Data.List.NonEmpty (NonEmpty)
import           Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

import           Data.String (String)

import           P

import           Prelude (fromEnum)

import           System.IO

import           Warden.Data
import           Warden.Error
import           Warden.Numeric
import           Warden.Row.Parser

import           X.Data.Conduit.Binary (slurp, sepByByteBounded)
import           X.Control.Monad.Trans.Either (EitherT)

decodeByteString :: Separator
                 -> LineBound
                 -> ViewFile
                 -> Conduit ByteString (EitherT WardenError (ResourceT IO)) Row
decodeByteString sep (LineBound lb) _vf =
      -- This deliberately doesn't try to handle different line-ending formats
      -- differently - RFC 4180-style files with CRLF line endings will have
      -- a junk \r at the end of each line, but this doesn't matter for
      -- validation purposes as long as it's consistent.
      -- FIXME: make the above actually true (numerics)
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

toRow :: Either String (Vector ByteString) -> Row
toRow (Right !rs) =
  -- Decode everything here for validation purposes, we won't have a chance
  -- to do it cleanly later.
  case T.decodeUtf8' (BS.concat $ V.toList rs) of
    Right _ ->
      SVFields rs
    Left !e ->
      RowFailure . T.pack $ show e
toRow (Left !e) =
  RowFailure $ T.pack e
{-# INLINE toRow #-}

-- | We only care about ASCII characters here (true, false et cetera)
-- and converting unicode to lowercase is really expensive, so just
-- add 32 to the character if it's in the ASCII uppercase range.
asciiToLower :: ByteString -> ByteString
asciiToLower = BS.map (flip (.|.) 0x20)
{-# INLINE asciiToLower #-}

parseField :: ByteString -> FieldLooks
parseField "" = LooksEmpty
parseField t = case AB.parseOnly fieldP (asciiToLower t) of
  Left _ -> LooksText
  Right ParsedIntegral -> LooksIntegral
  Right ParsedReal -> LooksReal
  Right ParsedBoolean -> LooksBoolean
{-# INLINE parseField #-}

updateFieldLooks :: ByteString -> VU.Vector ObservationCount -> VU.Vector ObservationCount
updateFieldLooks !t !a =
  VU.accum (+) a [(fromEnum (parseField t), 1)]
{-# INLINE updateFieldLooks #-}

updateTextCounts :: TextFreeformThreshold -> Row -> TextCounts -> TextCounts
updateTextCounts fft (SVFields vs) NoTextCounts =
  TextCounts $!! V.zipWith (updateUniqueTextCount fft) vs $
      V.replicate (V.length vs) emptyUniqueTextCount
updateTextCounts fft (SVFields vs) (TextCounts tcs) =
  TextCounts $!! V.zipWith (updateUniqueTextCount fft) vs tcs
updateTextCounts _ _ tc = tc
{-# INLINE updateTextCounts #-}

-- | Accumulator for field/row counts on tokenized raw data.
updateSVParseState :: TextFreeformThreshold
                   -> SVParseState
                   -> Row
                   -> SVParseState
updateSVParseState fft !st row =
  let good = countGood row
      bad  = countBad row  in
    (totalRows %~ ((good + bad) +))
  . (badRows %~ (bad +))
  . (numFields %~ (updateNumFields row))
  . (fieldLooks %~ (updateFields row))
  . (textCounts %~ (updateTextCounts fft row))
  . (numericState %~ (updateFieldNumericState row))
  $!! st
 where
  countGood (SVFields _)   = RowCount 1
  countGood (RowFailure _) = RowCount 0

  countBad (SVFields _)    = RowCount 0
  countBad (RowFailure _)  = RowCount 1
{-# INLINE updateSVParseState #-}

updateNumFields :: Row -> Set FieldCount -> Set FieldCount
updateNumFields (SVFields !v) !ns =
  let n = FieldCount $ V.length v in
  S.insert n ns
updateNumFields _ !ns = ns
{-# INLINE updateNumFields #-}

updateFields :: Row -> FieldLookCount -> FieldLookCount
updateFields (SVFields !v) NoFieldLookCount =
  FieldLookCount $ V.zipWith updateFieldLooks v $
    V.replicate (V.length v) emptyLookCountVector
updateFields (SVFields !v) (FieldLookCount !a) =
  FieldLookCount $!! V.zipWith updateFieldLooks v a
updateFields _ !a = a
{-# INLINE updateFields #-}

updateFieldNumericState :: Row -> FieldNumericState -> FieldNumericState
updateFieldNumericState (SVFields !v) NoFieldNumericState =
  FieldNumericState $ V.zipWith updateFieldNumericState' v $
    V.replicate (V.length v) initialNumericState
updateFieldNumericState (SVFields !v) (FieldNumericState !a) =
  FieldNumericState $!! V.zipWith updateFieldNumericState' v a
updateFieldNumericState _ !a = a
{-# INLINE updateFieldNumericState #-}

-- FIXME: parsing fields twice
updateFieldNumericState' :: ByteString -> NumericState -> NumericState
updateFieldNumericState' t !acc =
  case AB.parseOnly numericFieldP t of
    Left _ ->
      acc
    Right (NumericField n) ->
      updateNumericState acc n
{-# INLINE updateFieldNumericState' #-}

combineSVParseState :: TextFreeformThreshold -> SVParseState -> SVParseState -> SVParseState
combineSVParseState fft s !acc =
    (badRows %~ ((s ^. badRows) +))
  . (totalRows %~ ((s ^. totalRows) +))
  . (numFields %~ ((s ^. numFields) `S.union`))
  . (fieldLooks %~ ((s ^. fieldLooks) `combineFieldLooks`))
  . (textCounts %~ ((s ^. textCounts) `combineTextCounts'`))
  . (numericState %~ ((s ^. numericState) `combineFieldNumericState`))
  $! acc
  where
    combineTextCounts' = combineTextCounts fft
{-# INLINE combineSVParseState #-}

resolveSVParseState :: TextFreeformThreshold -> [SVParseState] -> SVParseState
resolveSVParseState fft = foldr (combineSVParseState fft) initialSVParseState
{-# INLINE resolveSVParseState #-}
