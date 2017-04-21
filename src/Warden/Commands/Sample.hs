{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Warden.Commands.Sample (
    extractNumericFields
  , summariseNumericFields
  , readNumericSummary
  , identical

  , SummaryStatsRecord(..)
) where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Resource (ResourceT)

import qualified Data.ByteString as BS
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

import           Delorean.Local.Date (renderDate)

import           P

import           System.FilePath (joinPath)
import           System.IO (IO, FilePath, Handle, IOMode(..))
import           System.IO (withFile)

import           Warden.Data.Marker
import           Warden.Data.Numeric
import           Warden.Data.Sampling
import           Warden.Data.View
import           Warden.Error
import           Warden.Marker

import           X.Control.Monad.Trans.Either (EitherT)
import           X.Control.Monad.Trans.Either (hoistEither)
import qualified X.Data.Vector.Generic as XV

summariseNumericFields
  :: FilePath
  -> [FilePath]
  -> EitherT WardenError (ResourceT IO) ()
summariseNumericFields outd fs =
  case nonEmpty fs of
    Nothing ->
      pure ()
    Just fs' -> do
      ms <- mapM readViewMarker fs'
      v <- hoistEither . first WardenSampleError $ resolveView ms
      nss <- hoistEither . first WardenSampleError $
        validateNumericSummaries =<< mapM (uncurry getNumericSummary) (NE.zip fs' ms)

      let
        -- Find the earliest and the latest dates covered by each marker (can
        -- overlap).
        dates = fmap (dateRange . vmDates . vmMetadata) ms
        -- Decorate each feed's summaries with the applicable date range.
        summaries = fmap (uncurry summariseStats) $ NE.zip dates nss
        -- Munge to row-major for serialising as CSV.
        summariesT = XV.transpose . V.fromList $ NE.toList summaries

      V.mapM_ (uncurry (writeFieldSummaryStats outd)) $ nameFields v summariesT

extractNumericFields
  :: FilePath
  -> [FilePath]
  -> EitherT WardenError (ResourceT IO) ()
extractNumericFields outp fs =
  case nonEmpty fs of
    Nothing ->
      pure ()
    Just fs' -> do
      nss <- mapM readNumericSummary fs'
      ss <- hoistEither . first WardenSampleError $ combineMarkerSamples nss
      writeSamples outp ss

-- FIXME: actual names
nameFields
  :: View
  -> V.Vector (V.Vector SummaryStatsRecord)
  -> V.Vector (Text, (V.Vector SummaryStatsRecord))
nameFields (View v) xs =
  let
    name ix x = (T.concat ["summary_", T.pack v, "_", renderIntegral ix], x)
  in
  V.imap name xs

writeFieldSummaryStats
  :: FilePath
  -> Text
  -> V.Vector SummaryStatsRecord
  -> EitherT WardenError (ResourceT IO) ()
writeFieldSummaryStats d name xs =
  let
    fp = joinPath [d, T.unpack $ name <> ".csv"]
  in
  liftIO $ withFile fp WriteMode $ \h ->
    V.mapM_ (BS.hPut h . T.encodeUtf8 . renderSummaryStatsRecord) xs

summariseStats
  :: DateRange
  -> V.Vector NumericSummary
  -> V.Vector SummaryStatsRecord
summariseStats dr ss =
  let
    summary NoNumericSummary = Nothing
    summary (NumericSummary mn mx mu sigma med _) = Just $ SummaryStatsRecord dr mn mx mu sigma med
  in
  XV.mapMaybe summary ss

writeSamples
  :: FilePath
  -> V.Vector Sample
  -> EitherT WardenError (ResourceT IO) ()
writeSamples fp ss =
  let
    ssT = transposeSamples ss
  in
  liftIO $ withFile fp WriteMode $ \h ->
    V.mapM_ (writeRow h) ssT

writeRow
  :: Handle
  -> VU.Vector Double
  -> IO ()
writeRow h xs =
  let
    row = T.encodeUtf8 . T.intercalate "," . fmap renderFractional $ VU.toList xs
  in do
  BS.hPut h row
  BS.hPut h "\n"

-- | Transpose sample vector from column-major (vector of samples, as
-- they're stored in the view metadata) to row-major (vector of
-- records) for output.
transposeSamples :: V.Vector Sample -> V.Vector (VU.Vector Double)
transposeSamples ss =
  let
    reify (Sample xs) = Just xs
    reify NoSample = Nothing
  in
  XV.transpose $ XV.mapMaybe reify ss

readNumericSummary
  :: FilePath
  -> EitherT WardenError (ResourceT IO) (V.Vector NumericSummary)
readNumericSummary f =
  hoistEither . first WardenSampleError . (getNumericSummary f) =<< readViewMarker f

getNumericSummary
  :: FilePath
  -> ViewMarker
  -> Either SampleError (V.Vector NumericSummary)
getNumericSummary f m =
  case rcsNumericSummaries (vmViewCounts $ vmMetadata m) of
    NoNumericFieldSummary ->
      Left $ NoNumericSummaries f
    NumericFieldSummary ss ->
      pure ss

combineMarkerSamples
  :: NonEmpty (V.Vector NumericSummary)
  -> Either SampleError (V.Vector Sample)
combineMarkerSamples nss =
  (reifySamples nss) >>= (pure . combineFieldSamples)

combineFieldSamples
  :: NonEmpty (V.Vector Sample)
  -> V.Vector Sample
combineFieldSamples (s:|ss) =
  foldl' (V.zipWith combineSamples) s ss

-- This isn't a monoid instance or implemented in Warden.Data.Sample
-- because I think it's wrong to do this without a shuffle; we do it here
-- because we know we shuffle at the end and don't need to do it on every
-- append.
combineSamples
  :: Sample
  -> Sample
  -> Sample
combineSamples NoSample x =
  x
combineSamples x NoSample =
  x
combineSamples (Sample x) (Sample y) =
  Sample $ x VU.++ y

reifySamples
  :: NonEmpty (V.Vector NumericSummary)
  -> Either SampleError (NonEmpty (V.Vector Sample))
reifySamples =
  let
    sample NoNumericSummary = Nothing
    sample (NumericSummary _ _ _ _ _ samp) = Just samp
  in
  pure . fmap (XV.mapMaybe sample) <=< validateNumericSummaries

validateNumericSummaries
  :: NonEmpty (V.Vector NumericSummary)
  -> Either SampleError (NonEmpty (V.Vector NumericSummary))
validateNumericSummaries nss =
  case identical (NE.toList $ numericIndicatorVec <$> nss) of
    False ->
      Left NumericFieldMismatch
    True ->
      pure nss

resolveView
  :: NonEmpty ViewMarker
  -> Either SampleError View
resolveView vms =
  let
    (v:|vs) = vmView <$> vms
  in
  case identical (v : vs) of
    True ->
      pure v
    False ->
      Left ViewMismatch

identical
  :: Eq a
  => [a]
  -> Bool
identical [] =
  True
identical (x:xs) =
  let
    go _ [] = True
    go z [y] = y == z
    go z (y:ys) = and [y == z, go y ys]
  in
  go x xs 

numericIndicatorVec
  :: V.Vector NumericSummary
  -> V.Vector Bool
numericIndicatorVec ns =
  flip V.map ns $ \x -> case x of
    NoNumericSummary ->
      False
    NumericSummary _ _ _ _ _ _ ->
      True

data SummaryStatsRecord =
  SummaryStatsRecord {
    summaryDates :: !DateRange
  , summaryMinimum :: !Minimum
  , summaryMaximum :: !Maximum
  , summaryMean :: !Mean
  , summaryStdDev :: !StdDev
  , summaryMedian :: !Median
  } deriving (Eq, Show)

renderSummaryStatsRecord
  :: SummaryStatsRecord
  -> Text
renderSummaryStatsRecord (SummaryStatsRecord dr mn mx mu sigma med) =
  let
    missing = "NA"

    d0 NoDates = missing
    d0 (DateRange x _y) = renderDate x

    d1 NoDates = missing
    d1 (DateRange _x y) = renderDate y

    fields = [
               d0 dr
             , d1 dr
             , renderMinimum missing mn
             , renderMaximum missing mx
             , renderMean missing mu
             , renderStdDev missing sigma
             , renderMedian missing med
             ]
  in
  T.intercalate "," fields <> "\n"
