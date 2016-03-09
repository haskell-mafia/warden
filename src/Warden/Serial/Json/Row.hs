{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Warden.Serial.Json.Row(
    fromFieldCount
  , toFieldCount
  , fromSVParseState
  , toSVParseState
  , toFieldVector
  , fromFieldVector
  , toLineBound
  , fromLineBound
  , toSeparator
  , fromSeparator
  ) where

import           Control.Monad.ST (runST)

import           Data.Aeson ((.:), (.:?), (.=), object, parseJSON, toJSON)
import           Data.Aeson.Types (Value(..), Parser, typeMismatch)
import           Data.Char (chr, ord)
import qualified Data.Set as S
import qualified Data.Text as T
import           Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Insertion as Insertion

import           P

import           Prelude (toEnum, fromEnum)

import           Warden.Data.Row
import           Warden.Serial.Json.Field

fromSeparator :: Separator -> Value
fromSeparator (Separator s) = String . T.pack . pure . chr $ fromIntegral s

toSeparator :: Value -> Parser Separator
toSeparator (String s) = case T.unpack s of
  [s'] -> pure . Separator . fromIntegral $ ord s'
  _ -> fail $ "invalid separator: " <> T.unpack s
toSeparator x = typeMismatch "Warden.Data.Row.Separator" x

fromLineBound :: LineBound -> Value
fromLineBound (LineBound n) = toJSON n

toLineBound :: Value -> Parser LineBound
toLineBound (Number n) = LineBound <$> parseJSON (Number n)
toLineBound x = typeMismatch "Warden.Data.Row.LineBound" x

fromRowCount :: RowCount -> Value
fromRowCount (RowCount n) = toJSON n

toRowCount :: Value -> Parser RowCount
toRowCount (Number n) = RowCount <$> parseJSON (Number n)
toRowCount x          = typeMismatch "Warden.Data.Row.RowCount" x

fromFieldCount :: FieldCount -> Value
fromFieldCount (FieldCount n) = toJSON n

toFieldCount :: Value -> Parser FieldCount
toFieldCount (Number n) = FieldCount <$> parseJSON (Number n)
toFieldCount x          = typeMismatch "Warden.Data.Row.FieldCount" x

fromFieldVector :: Vector ObservationCount -> Value
fromFieldVector a =
  toJSON $ V.imap fromAssoc a
  where
    fromAssoc i v = object [
        "looks" .= fromFieldLooks (toEnum i)
      , "count" .= toJSON (unObservationCount v)
      ]

toFieldVector :: Value -> Parser (Vector ObservationCount)
toFieldVector (Array os) = do
  assocs <- V.mapM toAssoc os
  pure . V.map snd $ runST $ do
    assocs' <- V.thaw assocs
    -- Insertion sort because the vectors are very small.
    Insertion.sort assocs'
    V.freeze assocs'
  where
    toAssoc (Object o) = do
      looks <- fmap fromEnum $ toFieldLooks =<< (o .: "looks")
      count' <- fmap ObservationCount $ parseJSON =<< (o .: "count")
      pure (looks, count')
    toAssoc x = typeMismatch "(Warden.Data.Row.FieldLooks, Integer)" x
toFieldVector x = typeMismatch "Vector Warden.Data.Row.FieldLooks Integer" x

fromSVParseState :: SVParseState -> Value
fromSVParseState (SVParseState br tr nfs fas) = object $ [
    "bad-rows" .= fromRowCount br
  , "total-rows" .= fromRowCount tr
  , "field-counts" .= (fmap fromFieldCount $ S.toList nfs)
  ] <> case fas of
         NoFieldLookCount ->
           []
         FieldLookCount fas' ->
           ["field-looks" .= (fmap fromFieldVector $ V.toList fas')]

toSVParseState :: Value -> Parser SVParseState
toSVParseState (Object o) = do
  br <- toRowCount =<< (o .: "bad-rows")
  tr <- toRowCount =<< (o .: "total-rows")
  nfs <- fmap S.fromList $ mapM toFieldCount =<< (o .: "field-counts")
  fas <- maybe (pure NoFieldLookCount) (fmap FieldLookCount . mapM toFieldVector) =<< (o .:? "field-looks")
  pure $ SVParseState br tr nfs fas
toSVParseState x          = typeMismatch "Warden.Data.Row.SVParseState" x
