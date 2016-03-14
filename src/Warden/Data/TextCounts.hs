{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

{-
Keep track of the number of Text values we've seen in a field by hashing them
and inserting into a Patricia tree.

Currently uses cityhash, which is a non-cryptographic hash function; if 
DoS prevention becomes necessary, SipHash is a good alternative.
-}
module Warden.Data.TextCounts (
    UniqueTextCount(..)
  , TextCounts(..)
  , TextFreeformThreshold(..)
  , hashText
  , updateUniqueTextCount
  ) where

import           Data.Digest.CityHash (cityHash64)
import           Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import           Data.Text (Text)
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V

import           GHC.Generics (Generic)

import           P

data UniqueTextCount =
    UniqueTextCount !IntSet
  | LooksFreeform
  deriving (Eq, Show, Generic)

instance NFData UniqueTextCount

newtype TextFreeformThreshold =
  TextFreeformThreshold {
    unTextFreeformThreshold :: Int
  } deriving (Eq, Show)

data TextCounts =
    TextCounts !(V.Vector UniqueTextCount)
  | NoTextCount
  deriving (Eq, Show, Generic)

instance NFData TextCounts

-- Probably want to make this configurable.
textFreeformThreshold :: TextFreeformThreshold
textFreeformThreshold = TextFreeformThreshold 1000

-- | Don't use this on 32-bit platforms.
--
-- Speed this up by replacing 'encodeUtf8' with a straight memcpy.
hashText :: Text -> Int
hashText t =
  fromIntegral . cityHash64 $ T.encodeUtf8 t

updateUniqueTextCount :: UniqueTextCount -> Text -> UniqueTextCount
updateUniqueTextCount LooksFreeform _ = LooksFreeform
updateUniqueTextCount (UniqueTextCount c) t
  | IS.size c >= (unTextFreeformThreshold textFreeformThreshold) =
      LooksFreeform
  | otherwise =
      let h = hashText t in
      UniqueTextCount $ IS.insert h c
