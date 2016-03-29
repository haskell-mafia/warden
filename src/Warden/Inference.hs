{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

{- Most of this module will probably end up in brandix. -}

module Warden.Inference (
    compatibleEntries
  , countCompatibleFields
  , countsForField
  , countsForType
  , generateSchema
  , inferField
  , inferForms
  , fieldCandidates
  , fieldLookSum
  , normalizeFieldHistogram
  , textCountSum
  , totalViewRows
  , validateViewMarkers
  , viewMarkerMismatch
  ) where

import           Control.Lens ((^.), view)

import           Data.List (zip)
import           Data.List.NonEmpty (NonEmpty(..), nonEmpty)
import qualified Data.List.NonEmpty as NE
import           Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

import           P

import           Warden.Data
import           Warden.Error

-- | Do these two markers look like they're compatible?
viewMarkerMismatch :: ViewMarker -> ViewMarker -> Either ValidationFailure ()
viewMarkerMismatch a b = do
  validateVersion (vmVersion a) (vmVersion b)
  validateView (vmView a) (vmView b)
  validateTotalFields (fields' a) (fields' b)
  validateFreeformThreshold (fft' a) (fft' b)
  where
    validateVersion = validateEq "vmVersion"

    validateView = validateEq "vmView"

    validateTotalFields = validateEq "numFields"

    validateFreeformThreshold = validateEq "checkFreeformThresholds"

    validateEq ctx x y =
      if x == y
        then pure ()
        else Left $ ViewMarkerMismatch ctx (T.pack $ show x) (T.pack $ show y)

    fields' vm' = (vmViewCounts $ vmMetadata vm') ^. numFields

    fft' = checkFreeformThreshold . vmCheckParams . vmMetadata

-- | Sanity-check view markers to prevent human error, e.g., trying to run
-- `infer` on markers from multiple views.
validateViewMarkers :: NonEmpty ViewMarker -> Either InferenceError ()
validateViewMarkers (m:|ms) = go m ms >> checkFails
  where
    go _ [] = pure ()
    go prev !(m':ms') = case viewMarkerMismatch prev m' of
      Right () -> go m' ms'
      Left f -> Left $ MarkerValidationFailure f

    checkFails =
      let markers = m:ms
          stati = join $ (fmap summaryStatus . vmCheckResults) <$> markers in
      case nonEmpty (filter ((/= MarkerPass) . snd) $ zip markers stati) of
        Nothing -> pure ()
        Just fs -> Left . MarkerValidationFailure . ChecksMarkedFailed $ (wpRunId . vmWardenParams . fst) <$> fs

fieldLookSum :: NonEmpty ViewMarker -> FieldLookCount
fieldLookSum =
  foldl' combineFieldLooks NoFieldLookCount . 
    fmap (view fieldLooks . vmViewCounts . vmMetadata)

textCountSum :: NonEmpty ViewMarker -> TextCounts
textCountSum vms =
  -- FFTs already validated as the same
  -- TODO: enforce this with types
  let fft = checkFreeformThreshold . vmCheckParams . vmMetadata $ NE.head vms in
  foldl' (combineTextCounts fft) NoTextCounts $
    fmap (view textCounts . vmViewCounts . vmMetadata) vms

inferForms :: NonEmpty ViewMarker -> Either InferenceError TextCountSummary
inferForms vms =
  let totalRowCount = totalViewRows vms
      fft = vmFFT $ NE.head vms in do
  when ((unRowCount totalRowCount) < (fromIntegral $ unTextFreeformThreshold fft)) $
    Left $ InsufficientRowsForFormInference totalRowCount fft
  case textCountSum vms of
    NoTextCounts -> Left NoTextCountError
    TextCounts cs -> fmap TextCountSummary $ V.mapM (uncurry summarizeTextCount) $ V.indexed cs
  where
    vmFFT = checkFreeformThreshold . vmCheckParams . vmMetadata

summarizeTextCount :: Int -> UniqueTextCount -> Either InferenceError FieldForm
summarizeTextCount _ LooksFreeform =
  pure FreeForm
summarizeTextCount i (UniqueTextCount hashes) =
  if S.null hashes
    then Left $ NoTextCountForField i
    else pure . CategoricalForm . FieldUniques $ S.size hashes

countsForField :: VU.Vector ObservationCount -> FieldHistogram
countsForField os =
  FieldHistogram $ VU.map (countsForType os) $ VU.fromList [minBound..maxBound]

countsForType :: VU.Vector ObservationCount -> FieldType -> CompatibleEntries
countsForType os t =
  VU.sum $ VU.zipWith (compatibleEntries t) (VU.fromList [minBound..maxBound]) os

compatibleEntries :: FieldType -> FieldLooks -> ObservationCount -> CompatibleEntries
compatibleEntries t l o =
  if fieldTypeIncludes t l
    then CompatibleEntries $ unObservationCount o
    else CompatibleEntries 0

countCompatibleFields :: NonEmpty ViewMarker -> Either InferenceError (V.Vector FieldHistogram)
countCompatibleFields vms = do
  validateViewMarkers vms
  case fieldLookSum vms of
      NoFieldLookCount ->
        Left $ MarkerValidationFailure NoFieldCounts
      FieldLookCount fls ->
        Right $ V.map countsForField fls

totalViewRows :: NonEmpty ViewMarker -> RowCount
totalViewRows = sum . fmap (view totalRows . vmViewCounts . vmMetadata)

normalizeFieldHistogram :: RowCount -> FieldHistogram -> Either InferenceError (VU.Vector NormalizedEntries)
normalizeFieldHistogram (RowCount rc) (FieldHistogram cs) = do
  when (rc == 0) $
    Left ZeroRowCountError
  case greaterThanRowCount of
    [] ->
      pure $ VU.map normalize' cs
    xs ->
      Left $ CompatibleFieldsGTRowCount (RowCount rc) xs
  where
    greaterThanRowCount =
      VU.toList . VU.map CompatibleEntries . VU.filter (> rc) $
        VU.map unCompatibleEntries cs

    normalize' (CompatibleEntries c) =
      NormalizedEntries $ (fromIntegral c) / (fromIntegral rc)

fieldCandidates :: FieldMatchRatio
                -> RowCount
                -> FieldHistogram
                -> Either InferenceError (Set FieldType)
fieldCandidates (FieldMatchRatio fmr) totalRowCount h = do
  normed <- normalizeFieldHistogram totalRowCount h
  if VU.null normed
    then Left $ EmptyFieldHistogram
    else
      let cands = VU.filter (aboveMatchThreshold . snd) $
                    VU.zip (VU.fromList $ [minBound..maxBound]) normed in
      pure . S.fromList . VU.toList $ VU.map fst cands
  where
    aboveMatchThreshold (NormalizedEntries n) =
      n >= fmr

inferField :: FieldMatchRatio
           -> RowCount
           -> FieldIndex
           -> FieldHistogram
           -> Either InferenceError FieldType
inferField fmr totalRowCount ix h = do
  fcs <- fieldCandidates fmr totalRowCount h
  case S.toList (minima fcs) of
    [] -> Left NoMinimalFieldTypes
    [ft] -> pure ft
    fts -> Left $ CannotResolveCandidates ix fts

generateSchema :: FieldMatchRatio
               -> TextCountSummary
               -> RowCount
               -> V.Vector FieldHistogram
               -> Either InferenceError Schema
generateSchema fmr (TextCountSummary ffs) totalRowCount hs = do
  fts <- V.mapM (uncurry (inferField fmr totalRowCount)) .
    V.map (first FieldIndex) $ V.indexed hs
  pure . Schema currentSchemaVersion $ V.zipWith SchemaField fts ffs
