{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Warden.Row (
    combineSVParseState
  , decodeByteString
  , parseField
  , readView
  , readView'
  , readViewChunk
  , readViewChunk'
  , readViewFile
  , resolveSVParseState
  , toRow
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
import qualified Data.ByteString as BS
import           Data.ByteString (ByteString)
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
import           Warden.Parser.Field
import           Warden.Parser.Row
import           Warden.PII
import           Warden.Sampling.Reservoir

import           X.Data.Conduit.Binary (slurp, sepByByteBounded)
import           X.Control.Monad.Trans.Either (EitherT)

decodeByteString :: FileFormat
                 -> Separator
                 -> LineBound
                 -> ViewFile
                 -> Conduit ByteString (EitherT WardenError (ResourceT IO)) Row
decodeByteString ff sep (LineBound lb) _vf = {-# SCC decodeByteString #-}
      sepByByteBounded (fromIntegral $ ord '\n') lb
  =$= decodeByteString'
  =$= DC.map toRow
  where
    decodeByteString' = awaitForever $ \l ->
      yield . second unRawRecord $ AB.parseOnly ((parserFor ff) sep) l
    {-# INLINE decodeByteString' #-}
{-# INLINE decodeByteString #-}

decodeRecord :: FileFormat
             -> Separator
             -> LineBound
             -> ViewFile
             -> Conduit ByteString (EitherT WardenError (ResourceT IO)) Row
decodeRecord = {-# SCC decodeRecord #-} decodeByteString
{-# INLINE decodeRecord #-}

readView :: FileFormat
         -> Separator
         -> LineBound
         -> NonEmpty ViewFile
         -> Source (EitherT WardenError (ResourceT IO)) Row
readView ff sep lb vfs =
  readView' (decodeRecord ff sep lb) vfs

readView' :: (ViewFile -> Conduit ByteString (EitherT WardenError (ResourceT IO)) Row)
          -> NonEmpty ViewFile
          -> Source (EitherT WardenError (ResourceT IO)) Row
readView' c vfs =
  sequence_ $ (readViewFile' c) <$> vfs

readViewFile :: FileFormat
             -> Separator
             -> LineBound
             -> ViewFile
             -> Source (EitherT WardenError (ResourceT IO)) Row
readViewFile ff sep lb =
  readViewFile' (decodeRecord ff sep lb)

readViewFile' :: (ViewFile -> Conduit ByteString (EitherT WardenError (ResourceT IO)) Row)
              -> ViewFile
              -> Source (EitherT WardenError (ResourceT IO)) Row
readViewFile' c vf =
  let fp = viewFilePath vf in
  slurp fp 0 Nothing 
    =$= c vf

readViewChunk :: FileFormat
              -> Separator
              -> LineBound
              -> ViewFile
              -> Chunk
              -> Source (EitherT WardenError (ResourceT IO)) Row
readViewChunk ff sep lb =
  readViewChunk' (decodeRecord ff sep lb)

readViewChunk' :: (ViewFile -> Conduit ByteString (EitherT WardenError (ResourceT IO)) Row)
               -> ViewFile
               -> Chunk
               -> Source (EitherT WardenError (ResourceT IO)) Row
readViewChunk' c vf (Chunk offset size) =
  let fp = viewFilePath vf in
  slurp fp (unChunkOffset offset) (Just $ unChunkSize size)
    =$= c vf

toRow :: Either String (Vector ByteString) -> Row
toRow (Right !rs) = {-# SCC toRow #-}
  -- Decode everything here for validation purposes, we won't have a chance
  -- to do it cleanly later.
  --
  -- We concatenate the fields before decoding for speed reasons, but we
  -- intercalate the null byte between them - as this byte is a valid ASCII
  -- character, it's not part of any valid multibyte unicode code sequence.
  -- This avoids the error case encountered when two individually-invalid
  -- sequences are concatenated to form a valid sequence.
  case T.decodeUtf8' (BS.intercalate "\0" $ V.toList rs) of
    Right _ ->
      SVFields rs
    Left !e ->
      RowFailure . T.pack $ show e
toRow (Left !e) = {-# SCC toRow #-}
  RowFailure $ T.pack e
{-# INLINE toRow #-}

-- | Try parsing as all the specific field types. If none work, we default
-- to 'LooksText'.
parseField :: ByteString -> FieldLooks
parseField "" = {-# SCC parseField #-}
  LooksEmpty
parseField t = {-# SCC parseField #-}
  if checkFieldBool t
    then LooksBoolean
    else case checkFieldNumeric t of
      Nothing' -> LooksText
      Just' x -> x
{-# INLINE parseField #-}

updateFieldLooks :: FieldLooks -> VU.Vector ObservationCount -> VU.Vector ObservationCount
updateFieldLooks !fl !a = {-# SCC updateFieldLooks #-}
  VU.accum (+) a [(fromEnum fl, 1)]
{-# INLINE updateFieldLooks #-}

updateTextCounts :: TextFreeformThreshold -> Row -> TextCounts -> TextCounts
updateTextCounts fft (SVFields vs) NoTextCounts = {-# SCC updateTextCounts #-}
  TextCounts $!! V.zipWith (updateUniqueTextCount fft) vs $
      V.replicate (V.length vs) emptyUniqueTextCount
updateTextCounts fft (SVFields vs) (TextCounts tcs) = {-# SCC updateTextCounts #-}
  TextCounts $!! V.zipWith (updateUniqueTextCount fft) vs tcs
updateTextCounts _ _ tc = {-# SCC updateTextCounts #-} tc
{-# INLINE updateTextCounts #-}

-- | Accumulator for field/row counts on tokenized raw data.
--
-- This is the main workhorse of the core fold which produces the view
-- metadata - all the state that's serialized at the end of a check or
-- used to calculate the check status is updated here.
updateSVParseState :: TextFreeformThreshold
                   -> Gen (PrimState IO)
                   -> SamplingType
                   -> PIICheckType
                   -> SVParseState
                   -> Row
                   -> EitherT WardenError (ResourceT IO) SVParseState
updateSVParseState fft g sType pct !st row = {-# SCC updateSVParseState #-}
  let good = countGood row
      bad  = countBad row
      rc = good + bad + (st ^. totalRows)
      fns = st ^. numericState
      fra = st ^. reservoirState
      -- Hold on to the FieldLooks vector here so we can use it to
      -- distinguish fields which need numeric updates.
      (fls, flc) = updateFields row $ st ^. fieldLooks
      st' =  (totalRows %~ ((good + bad) +))
           . (badRows %~ (bad +))
           . (numFields %~ (updateNumFields row))
           . (fieldLooks .~ flc)
           . (textCounts %~ (updateTextCounts fft row))
           . (piiState %~ (updatePIIState pct row))
           $!! st in do
  -- Do all the numeric stuff at once so we don't have to determine if the
  -- values are numeric multiple times.
  (fns', fra') <- liftIO $ updateFieldNumerics g rc sType row fls fns fra
  pure $!! st' & numericState .~ fns' & reservoirState .~ fra'
  where
    countGood (SVFields _)   = RowCount 1
    countGood (RowFailure _) = RowCount 0

    countBad (SVFields _)    = RowCount 0
    countBad (RowFailure _)  = RowCount 1
{-# INLINE updateSVParseState #-}

updatePIIState :: PIICheckType -> Row -> PIIObservations -> PIIObservations
updatePIIState NoPIIChecks  _ ps = {-# SCC updatePIIState #-}
  ps
updatePIIState (PIIChecks mpo) (SVFields v) ps = {-# SCC updatePIIState #-}
  V.foldl update' ps $ V.indexed v
  where
    update' acc (ix, bs) =
      updatePIIObservations mpo (FieldIndex ix) acc bs
updatePIIState _ _ ps = {-# SCC updatePIIState #-}
  ps
{-# INLINE updatePIIState #-}

updateNumFields :: Row -> Set FieldCount -> Set FieldCount
updateNumFields (SVFields !v) !ns = {-# SCC updateNumFields #-}
  let n = FieldCount $ V.length v in
  S.insert n ns
updateNumFields _ !ns = {-# SCC updateNumFields #-} ns
{-# INLINE updateNumFields #-}

updateFields :: Row -> FieldLookCount -> (Maybe' (V.Vector FieldLooks), FieldLookCount)
updateFields (SVFields !v) NoFieldLookCount = {-# SCC updateFields #-}
  let ls = V.map parseField v
      flc = FieldLookCount $ V.zipWith updateFieldLooks ls $
        V.replicate (V.length v) emptyLookCountVector in
  (,) (Just' ls) $!! flc
updateFields (SVFields !v) (FieldLookCount !a) = {-# SCC updateFields #-}
  let ls = V.map parseField v
      flc = FieldLookCount $ V.zipWith updateFieldLooks ls a in
  (,) (Just' ls) $!! flc
updateFields _ !a = {-# SCC updateFields #-} (Nothing', a)
{-# INLINE updateFields #-}

updateFieldNumerics :: Gen (PrimState IO)
                    -> RowCount
                    -> SamplingType
                    -> Row
                    -> Maybe' (V.Vector FieldLooks)
                    -> FieldNumericState
                    -> FieldReservoirAcc
                    -> IO (FieldNumericState, FieldReservoirAcc)
updateFieldNumerics g rc st (SVFields v) fls fns fra = {-# SCC updateFieldNumerics #-}
  -- If we have FieldLooks results for these fields, use them as a hint to
  -- avoid parsing fields we know are not numeric.
  let ns = maybe' (V.map parseProbablyNumeric v) (V.zipWith isNumeric v) fls
      fns' = updateFieldNumericState ns fns in do
  fra' <- case st of
    NoSampling -> pure fra
    ReservoirSampling rs -> updateFieldReservoirAcc g rs rc ns fra
  pure (fns', fra')
  where
    isNumeric f fl =
      if looksNumeric fl
        then parseProbablyNumeric f
        else NoNumericField
    {-# INLINE isNumeric #-}

    parseProbablyNumeric f =
      toMNumericField $ AB.parseOnly numericFieldP f
    {-# INLINE parseProbablyNumeric #-}

    toMNumericField (Left _) = NoNumericField
    toMNumericField (Right x) = MNumericField x
    {-# INLINE toMNumericField #-}
updateFieldNumerics _ _ _ _ _ fns fra = {-# SCC updateFieldNumerics #-}
  pure (fns, fra)
{-# INLINE updateFieldNumerics #-}

updateFieldNumericState :: V.Vector MNumericField -> FieldNumericState -> FieldNumericState
updateFieldNumericState ns NoFieldNumericState = {-# SCC updateFieldNumericState #-}
  FieldNumericState $ V.zipWith updateFieldNumericState' ns $
    V.replicate (V.length ns) initialNumericState
updateFieldNumericState ns (FieldNumericState !a) = {-# SCC updateFieldNumericState #-}
  FieldNumericState $!! V.zipWith updateFieldNumericState' ns a
{-# INLINE updateFieldNumericState #-}

updateFieldNumericState' :: MNumericField -> NumericState -> NumericState
updateFieldNumericState' mn !acc = {-# SCC updateFieldNumericState' #-}
  case mn of
    NoNumericField ->
      acc
    MNumericField (NumericField n) ->
      updateNumericState acc n
{-# INLINE updateFieldNumericState' #-}

updateFieldReservoirAcc :: Gen (PrimState IO)
                        -> ReservoirSize
                        -> RowCount
                        -> V.Vector MNumericField
                        -> FieldReservoirAcc
                        -> IO FieldReservoirAcc
updateFieldReservoirAcc g rs rc ns NoFieldReservoirAcc = {-# SCC updateFieldReservoirAcc #-}
  fmap FieldReservoirAcc $ V.zipWithM (updateFieldReservoirAcc' g rs rc) ns $
    V.replicate (V.length ns) NoReservoirAcc
updateFieldReservoirAcc g rs rc ns (FieldReservoirAcc !a) = {-# SCC updateFieldReservoirAcc #-}
  fmap FieldReservoirAcc $ V.zipWithM (updateFieldReservoirAcc' g rs rc) ns a
{-# INLINE updateFieldReservoirAcc #-}

updateFieldReservoirAcc' :: Gen (PrimState IO)
                         -> ReservoirSize
                         -> RowCount
                         -> MNumericField
                         -> ReservoirAcc
                         -> IO ReservoirAcc
updateFieldReservoirAcc' g rs rc mn !acc = {-# SCC updateFieldReservoirAcc' #-}
  case mn of
    NoNumericField ->
      pure acc
    MNumericField (NumericField n) ->
      updateReservoirAcc g rs rc acc n
{-# INLINE updateFieldReservoirAcc' #-}

combineSVParseState :: TextFreeformThreshold
                    -> Gen (PrimState IO)
                    -> SamplingType
                    -> PIICheckType
                    -> SVParseState
                    -> SVParseState
                    -> IO SVParseState
combineSVParseState fft g st pct s !acc = {-# SCC combineSVParseState #-}
  let acc' =  (badRows %~ ((s ^. badRows) +))
            . (totalRows %~ ((s ^. totalRows) +))
            . (numFields %~ ((s ^. numFields) `S.union`))
            . (fieldLooks %~ ((s ^. fieldLooks) `combineFieldLooks`))
            . (textCounts %~ ((s ^. textCounts) `combineTextCounts'`))
            . (numericState %~ ((s ^. numericState) `combineFieldNumericState`))
            . (piiState %~ ((s ^. piiState) `combinePIIObservations'`))
            $!! acc in case st of
  NoSampling ->
    pure acc'
  ReservoirSampling rs -> do
    fra' <- combineFieldReservoirAcc g rs (s ^. reservoirState) (acc ^. reservoirState)
    pure $ acc' & reservoirState .~ fra'
  where
    combineTextCounts' = combineTextCounts fft
    {-# INLINE combineTextCounts' #-}

    combinePIIObservations' x y =
      case pct of
        NoPIIChecks -> NoPIIObservations
        PIIChecks mpo -> combinePIIObservations mpo x y
    {-# INLINE combinePIIObservations' #-}
{-# INLINE combineSVParseState #-}

resolveSVParseState :: TextFreeformThreshold
                    -> Gen (PrimState IO)
                    -> SamplingType
                    -> PIICheckType
                    -> [SVParseState]
                    -> IO SVParseState
resolveSVParseState fft g st pct = {-# SCC resolveSVParseState #-}
  foldM (combineSVParseState fft g st pct) initialSVParseState
{-# INLINE resolveSVParseState #-}

combineFieldReservoirAcc :: Gen (PrimState IO)
                         -> ReservoirSize
                         -> FieldReservoirAcc
                         -> FieldReservoirAcc
                         -> IO FieldReservoirAcc
combineFieldReservoirAcc _g _rs NoFieldReservoirAcc NoFieldReservoirAcc = {-# SCC combineFieldReservoirAcc #-}
  pure NoFieldReservoirAcc
combineFieldReservoirAcc _g _rs NoFieldReservoirAcc y = {-# SCC combineFieldReservoirAcc #-}
  pure y
combineFieldReservoirAcc _g _rs x NoFieldReservoirAcc = {-# SCC combineFieldReservoirAcc #-}
  pure x
combineFieldReservoirAcc g rs (FieldReservoirAcc xs) (FieldReservoirAcc ys) = {-# SCC combineFieldReservoirAcc #-}
  fmap FieldReservoirAcc $ V.zipWithM (combineReservoirAcc g rs) xs ys
{-# INLINE combineFieldReservoirAcc #-}
