{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

{-
Keep track of the number of Text values we've seen in a field by hashing them
and inserting into a balanced binary tree (faster than the PATRICIA tree
in Data.IntSet).

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
  , textFreeformThreshold
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

-- Probably want to make this configurable.
textFreeformThreshold :: TextFreeformThreshold
textFreeformThreshold = TextFreeformThreshold 100

-- | Don't use this on 32-bit platforms.
--
-- Speed this up by replacing 'encodeUtf8' with a straight memcpy.
hashText :: Text -> Int
hashText t =
  fromIntegral . cityHash64 $ T.encodeUtf8 t
{-# INLINE hashText #-}

updateUniqueTextCount :: Text -> UniqueTextCount -> UniqueTextCount
updateUniqueTextCount _ LooksFreeform = LooksFreeform
updateUniqueTextCount t (UniqueTextCount c)
  | S.size c >= (unTextFreeformThreshold textFreeformThreshold) =
      LooksFreeform
  | otherwise =
      let h = hashText t in
      UniqueTextCount $ S.insert h c
{-# INLINE updateUniqueTextCount #-}

combineTextCounts :: TextCounts -> TextCounts -> TextCounts
combineTextCounts NoTextCounts NoTextCounts = NoTextCounts
combineTextCounts NoTextCounts (TextCounts b) = TextCounts b
combineTextCounts (TextCounts a) NoTextCounts = TextCounts a
combineTextCounts (TextCounts a) (TextCounts b) = TextCounts $ combine'
  where
    combine' = V.zipWith combineUniqueTextCounts a b

combineUniqueTextCounts :: UniqueTextCount -> UniqueTextCount -> UniqueTextCount
combineUniqueTextCounts LooksFreeform LooksFreeform = LooksFreeform
combineUniqueTextCounts LooksFreeform (UniqueTextCount _) = LooksFreeform
combineUniqueTextCounts (UniqueTextCount _) LooksFreeform = LooksFreeform
combineUniqueTextCounts (UniqueTextCount a) (UniqueTextCount b) =
  let c = S.union a b in
  if S.size c >= (unTextFreeformThreshold textFreeformThreshold)
    then LooksFreeform
    else UniqueTextCount c
