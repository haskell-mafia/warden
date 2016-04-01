{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Warden.Serial.Json.Row(
    fromFieldCount
  , toFieldCount
  , fromRowCount
  , toRowCount
  , toFieldVector
  , fromFieldVector
  , toLineBound
  , fromLineBound
  , toSeparator
  , fromSeparator
  ) where

import           Control.Monad.ST (runST)

import           Data.Aeson ((.:), (.=), object, parseJSON, toJSON)
import           Data.Aeson.Types (Value(..), Parser, typeMismatch)
import           Data.Char (chr, ord)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Algorithms.Insertion as Insertion

import           P

import           Prelude (fromEnum)

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

fromFieldVector :: VU.Vector ObservationCount -> Value
fromFieldVector a =
  let ls = V.fromList $ [minBound..maxBound] in
  toJSON . V.zipWith fromAssoc ls $ VU.convert a
  where
    fromAssoc l v = object [
        "looks" .= fromFieldLooks l
      , "count" .= toJSON (unObservationCount v)
      ]

toFieldVector :: Value -> Parser (VU.Vector ObservationCount)
toFieldVector (Array os) = do
  assocs <- V.mapM toAssoc os
  pure . VU.map snd . VU.convert $ runST $ do
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
toFieldVector x = typeMismatch "V.Vector Warden.Data.Row.FieldLooks Integer" x
