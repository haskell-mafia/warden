{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Warden where

import qualified Data.ByteString as BS
import           Data.Char (ord)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

import           P

import           Warden.Data

sumFLC :: FieldLookCount -> ObservationCount
sumFLC l = sum . join $ VU.toList <$> (lookArrays l)
  where
    lookArrays l' = case l' of
      NoFieldLookCount -> []
      FieldLookCount v -> V.toList v

-- Need to strip the quotes from certain test data before comparison as our
-- parser can leave them quoted/duplicated.
stripFieldQuotes :: Row -> Row
stripFieldQuotes (SVFields fs) =
  SVFields $ V.map (BS.filter (/= (fromIntegral $ ord '"'))) fs
stripFieldQuotes x = x
