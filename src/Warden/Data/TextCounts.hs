{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

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
  , renderTextFreeformThreshold
  , renderUniqueTextCount
  , updateUniqueTextCount
  ) where

import           Control.DeepSeq.Generics (genericRnf)

import           Data.AEq (AEq, (===), (~==))
import           Data.ByteString (ByteString)
import           Data.Digest.CityHash (cityHash64)
import           Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V

import           GHC.Generics (Generic)

import           P

data UniqueTextCount =
    UniqueTextCount !(Set Int)
  | LooksFreeform
  deriving (Eq, Show, Generic)

instance NFData UniqueTextCount where rnf = genericRnf

instance AEq UniqueTextCount where
  (===) = (==)

  (~==) = (==)

emptyUniqueTextCount :: UniqueTextCount
emptyUniqueTextCount = UniqueTextCount S.empty

-- human-readable for failure summaries
renderUniqueTextCount :: UniqueTextCount -> Text
renderUniqueTextCount LooksFreeform = "above freeform threshold"
renderUniqueTextCount (UniqueTextCount us) = T.concat [
    renderIntegral (S.size us)
  , " uniques"
  ]

newtype TextFreeformThreshold =
  TextFreeformThreshold {
    unTextFreeformThreshold :: Int
  } deriving (Eq, Show, Generic)

instance NFData TextFreeformThreshold where rnf = genericRnf

renderTextFreeformThreshold :: TextFreeformThreshold -> Text
renderTextFreeformThreshold = renderIntegral . unTextFreeformThreshold

data TextCounts =
    TextCounts !(V.Vector UniqueTextCount)
  | NoTextCounts
  deriving (Eq, Show, Generic)

instance NFData TextCounts where rnf = genericRnf

-- | Don't use this on 32-bit platforms.
hashText :: ByteString -> Int
hashText = {-# SCC hashText #-}
  fromIntegral . cityHash64
{-# INLINE hashText #-}

updateUniqueTextCount :: TextFreeformThreshold -> ByteString -> UniqueTextCount -> UniqueTextCount
updateUniqueTextCount _ _ LooksFreeform = {-# SCC updateUniqueTextCount #-} LooksFreeform
updateUniqueTextCount fft t (UniqueTextCount c) = {-# SCC updateUniqueTextCount #-}
  if S.size c >= (unTextFreeformThreshold fft)
    then
      LooksFreeform
    else
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
