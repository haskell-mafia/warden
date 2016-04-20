{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Warden.Row (
    asciiToLower
  , combineSVParseState
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
  , updateFieldNumericState'
  , updateFieldReservoirAcc
  , updateFields
  , updateNumFields
  , updateTextCounts
  , updateSVParseState
  ) where

import           Control.Lens ((%~), (^.), (.~))
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Primitive (PrimMonad(..))
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
import           System.Random.MWC (Gen)

import           Warden.Data
import           Warden.Error
import           Warden.Numeric
import           Warden.Parser.Row
import           Warden.Sampling.Reservoir

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
#ifndef NOINLINE
{-# INLINE decodeByteString #-}
#endif

decodeRecord :: Separator
             -> LineBound
             -> ViewFile
             -> Conduit ByteString (EitherT WardenError (ResourceT IO)) Row
decodeRecord = decodeByteString
#ifndef NOINLINE
{-# INLINE decodeRecord #-}
#endif

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
#ifndef NOINLINE
{-# INLINE toRow #-}
#endif

-- | We only care about ASCII characters here (true, false et cetera)
-- and converting unicode to lowercase is really expensive, so just
-- bitwise-or with the case bit (2^5).
--
-- This will bork some characters (higher-range punctuation), but they're not
-- digits or bools so we don't care about them.
asciiToLower :: ByteString -> ByteString
asciiToLower = BS.map (flip (.|.) 0x20)
#ifndef NOINLINE
{-# INLINE asciiToLower #-}
#endif

parseField :: ByteString -> FieldLooks
parseField "" = LooksEmpty
parseField t = case AB.parseOnly fieldP (asciiToLower t) of
  Left _ -> LooksText
  Right ParsedIntegral -> LooksIntegral
  Right ParsedReal -> LooksReal
  Right ParsedBoolean -> LooksBoolean
#ifndef NOINLINE
{-# INLINE parseField #-}
#endif

updateFieldLooks :: ByteString -> VU.Vector ObservationCount -> VU.Vector ObservationCount
updateFieldLooks !t !a =
  VU.accum (+) a [(fromEnum (parseField t), 1)]
#ifndef NOINLINE
{-# INLINE updateFieldLooks #-}
#endif

updateTextCounts :: TextFreeformThreshold -> Row -> TextCounts -> TextCounts
updateTextCounts fft (SVFields vs) NoTextCounts =
  TextCounts $!! V.zipWith (updateUniqueTextCount fft) vs $
      V.replicate (V.length vs) emptyUniqueTextCount
updateTextCounts fft (SVFields vs) (TextCounts tcs) =
  TextCounts $!! V.zipWith (updateUniqueTextCount fft) vs tcs
updateTextCounts _ _ tc = tc
#ifndef NOINLINE
{-# INLINE updateTextCounts #-}
#endif

-- | Accumulator for field/row counts on tokenized raw data.
updateSVParseState :: TextFreeformThreshold
                   -> Gen (PrimState IO)
                   -> SamplingType
                   -> SVParseState
                   -> Row
                   -> EitherT WardenError (ResourceT IO) SVParseState
updateSVParseState fft g sType !st row =
  let good = countGood row
      bad  = countBad row
      rc = good + bad + (st ^. totalRows)
      fns = st ^. numericState
      fra = st ^. reservoirState
      st' =  (totalRows %~ ((good + bad) +))
           . (badRows %~ (bad +))
           . (numFields %~ (updateNumFields row))
           . (fieldLooks %~ (updateFields row))
           . (textCounts %~ (updateTextCounts fft row))
           $!! st in do
  -- Do all the numeric stuff at once so we don't have to determine if the
  -- values are numeric multiple times.
  (fns', fra') <- liftIO $ updateFieldNumerics g rc sType row fns fra
  pure $!! st' & numericState .~ fns' & reservoirState .~ fra'
  where
    countGood (SVFields _)   = RowCount 1
    countGood (RowFailure _) = RowCount 0

    countBad (SVFields _)    = RowCount 0
    countBad (RowFailure _)  = RowCount 1
#ifndef NOINLINE
{-# INLINE updateSVParseState #-}
#endif

updateNumFields :: Row -> Set FieldCount -> Set FieldCount
updateNumFields (SVFields !v) !ns =
  let n = FieldCount $ V.length v in
  S.insert n ns
updateNumFields _ !ns = ns
#ifndef NOINLINE
{-# INLINE updateNumFields #-}
#endif

updateFields :: Row -> FieldLookCount -> FieldLookCount
updateFields (SVFields !v) NoFieldLookCount =
  FieldLookCount $ V.zipWith updateFieldLooks v $
    V.replicate (V.length v) emptyLookCountVector
updateFields (SVFields !v) (FieldLookCount !a) =
  FieldLookCount $!! V.zipWith updateFieldLooks v a
updateFields _ !a = a
#ifndef NOINLINE
{-# INLINE updateFields #-}
#endif

-- FIXME: parsing fields twice
updateFieldNumerics :: Gen (PrimState IO)
                    -> RowCount
                    -> SamplingType
                    -> Row
                    -> FieldNumericState
                    -> FieldReservoirAcc
                    -> IO (FieldNumericState, FieldReservoirAcc)
updateFieldNumerics g rc st (SVFields v) fns fra =
  let ns = V.map (toMNumericField . AB.parseOnly numericFieldP) v
      fns' = updateFieldNumericState ns fns in do
  fra' <- case st of
    NoSampling -> pure fra
    ReservoirSampling rs -> updateFieldReservoirAcc g rs rc ns fra
  pure (fns', fra')
  where
    toMNumericField (Left _) = NoNumericField
    toMNumericField (Right x) = MNumericField x
updateFieldNumerics _ _ _ _ fns fra =
  pure (fns, fra)
#ifndef NOINLINE
{-# INLINE updateFieldNumerics #-}
#endif

updateFieldNumericState :: V.Vector MNumericField -> FieldNumericState -> FieldNumericState
updateFieldNumericState ns NoFieldNumericState =
  FieldNumericState $ V.zipWith updateFieldNumericState' ns $
    V.replicate (V.length ns) initialNumericState
updateFieldNumericState ns (FieldNumericState !a) =
  FieldNumericState $!! V.zipWith updateFieldNumericState' ns a
#ifndef NOINLINE
{-# INLINE updateFieldNumericState #-}
#endif

updateFieldNumericState' :: MNumericField -> NumericState -> NumericState
updateFieldNumericState' mn !acc =
  case mn of
    NoNumericField ->
      acc
    MNumericField (NumericField n) ->
      updateNumericState acc n
#ifndef NOINLINE
{-# INLINE updateFieldNumericState' #-}
#endif

updateFieldReservoirAcc :: Gen (PrimState IO)
                        -> ReservoirSize
                        -> RowCount
                        -> V.Vector MNumericField
                        -> FieldReservoirAcc
                        -> IO FieldReservoirAcc
updateFieldReservoirAcc g rs rc ns NoFieldReservoirAcc =
  fmap FieldReservoirAcc $ V.zipWithM (updateFieldReservoirAcc' g rs rc) ns $
    V.replicate (V.length ns) NoReservoirAcc
updateFieldReservoirAcc g rs rc ns (FieldReservoirAcc !a) =
  fmap FieldReservoirAcc $ V.zipWithM (updateFieldReservoirAcc' g rs rc) ns a
#ifndef NOINLINE
{-# INLINE updateFieldReservoirAcc #-}
#endif

updateFieldReservoirAcc' :: Gen (PrimState IO)
                         -> ReservoirSize
                         -> RowCount
                         -> MNumericField
                         -> ReservoirAcc
                         -> IO ReservoirAcc
updateFieldReservoirAcc' g rs rc mn !acc =
  case mn of
    NoNumericField ->
      pure acc
    MNumericField (NumericField n) ->
      updateReservoirAcc g rs rc acc n
#ifndef NOINLINE
{-# INLINE updateFieldReservoirAcc' #-}
#endif

combineSVParseState :: TextFreeformThreshold
                    -> Gen (PrimState IO)
                    -> SamplingType
                    -> SVParseState
                    -> SVParseState
                    -> IO SVParseState
combineSVParseState fft g st s !acc =
  let acc' =  (badRows %~ ((s ^. badRows) +))
            . (totalRows %~ ((s ^. totalRows) +))
            . (numFields %~ ((s ^. numFields) `S.union`))
            . (fieldLooks %~ ((s ^. fieldLooks) `combineFieldLooks`))
            . (textCounts %~ ((s ^. textCounts) `combineTextCounts'`))
            . (numericState %~ ((s ^. numericState) `combineFieldNumericState`))
            $!! acc in case st of
  NoSampling ->
    pure acc'
  ReservoirSampling rs -> do
    fra' <- combineFieldReservoirAcc g rs (s ^. reservoirState) (acc ^. reservoirState)
    pure $ acc' & reservoirState .~ fra'
  where
    combineTextCounts' = combineTextCounts fft
#ifndef NOINLINE
{-# INLINE combineSVParseState #-}
#endif

resolveSVParseState :: TextFreeformThreshold
                    -> Gen (PrimState IO)
                    -> SamplingType
                    -> [SVParseState]
                    -> IO SVParseState
resolveSVParseState fft g st = foldM (combineSVParseState fft g st) initialSVParseState
#ifndef NOINLINE
{-# INLINE resolveSVParseState #-}
#endif

combineFieldReservoirAcc :: Gen (PrimState IO)
                         -> ReservoirSize
                         -> FieldReservoirAcc
                         -> FieldReservoirAcc
                         -> IO FieldReservoirAcc
combineFieldReservoirAcc _g _rs NoFieldReservoirAcc NoFieldReservoirAcc =
  pure NoFieldReservoirAcc
combineFieldReservoirAcc _g _rs NoFieldReservoirAcc y =
  pure y
combineFieldReservoirAcc _g _rs x NoFieldReservoirAcc =
  pure x
combineFieldReservoirAcc g rs (FieldReservoirAcc xs) (FieldReservoirAcc ys) =
  fmap FieldReservoirAcc $ V.zipWithM (combineReservoirAcc g rs) xs ys
#ifndef NOINLINE
{-# INLINE combineFieldReservoirAcc #-}
#endif
