{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

{-
Keep track of the number of Text values we've seen in a field by hashing them
and inserting into a balanced binary tree (faster than the PATRICIA tree
in Data.IntSet, possibly due to sparse population of the hash space).

Currently uses cityhash, which is a non-cryptographic hash function; if 
DoS prevention becomes necessary, SipHash is a good alternative.
-}
module Warden.Data.TextCounts (
    UniqueTextCount(..)
  , TextCounts(..)
  , TextFreeformThreshold(..)
  , combineTextCounts
  , combineUniqueTextCounts
  , emptyUniqueTextCount
  , hashText
  , updateUniqueTextCount
  ) where

import           Data.Digest.CityHash (cityHash64)
import           Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V

import           GHC.Generics (Generic)

import           P

data UniqueTextCount =
    UniqueTextCount !(Set Int)
  | LooksFreeform
  deriving (Eq, Show, Generic)

instance NFData UniqueTextCount

emptyUniqueTextCount :: UniqueTextCount
emptyUniqueTextCount = UniqueTextCount S.empty

newtype TextFreeformThreshold =
  TextFreeformThreshold {
    unTextFreeformThreshold :: Int
  } deriving (Eq, Show)

data TextCounts =
    TextCounts !(V.Vector UniqueTextCount)
  | NoTextCounts
  deriving (Eq, Show, Generic)

instance NFData TextCounts

-- | Don't use this on 32-bit platforms.
--
-- Speed this up by replacing 'encodeUtf8' with a straight memcpy.
hashText :: Text -> Int
hashText t =
  fromIntegral . cityHash64 $ T.encodeUtf8 t
{-# INLINE hashText #-}

updateUniqueTextCount :: TextFreeformThreshold -> Text -> UniqueTextCount -> UniqueTextCount
updateUniqueTextCount _ _ LooksFreeform = LooksFreeform
updateUniqueTextCount fft t (UniqueTextCount c)
  | S.size c >= (unTextFreeformThreshold fft) =
      LooksFreeform
  | otherwise =
      let h = hashText t in
      UniqueTextCount $ S.insert h c
{-# INLINE updateUniqueTextCount #-}

combineTextCounts :: TextFreeformThreshold -> TextCounts -> TextCounts -> TextCounts
combineTextCounts _ NoTextCounts NoTextCounts = NoTextCounts
combineTextCounts _ NoTextCounts (TextCounts b) = TextCounts b
combineTextCounts _ (TextCounts a) NoTextCounts = TextCounts a
combineTextCounts fft (TextCounts a) (TextCounts b) = TextCounts $ combine'
  where
    combine' = V.zipWith (combineUniqueTextCounts fft) a b

combineUniqueTextCounts :: TextFreeformThreshold -> UniqueTextCount -> UniqueTextCount -> UniqueTextCount
combineUniqueTextCounts _ LooksFreeform LooksFreeform = LooksFreeform
combineUniqueTextCounts _ LooksFreeform (UniqueTextCount _) = LooksFreeform
combineUniqueTextCounts _ (UniqueTextCount _) LooksFreeform = LooksFreeform
combineUniqueTextCounts fft (UniqueTextCount a) (UniqueTextCount b) =
  let c = S.union a b in
  if S.size c >= (unTextFreeformThreshold fft)
    then LooksFreeform
    else UniqueTextCount c
