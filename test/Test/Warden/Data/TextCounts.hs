{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Warden.Data.TextCounts where

import           Data.ByteString (ByteString)
import qualified Data.Set as S
import           Data.List (take)
import qualified Data.Vector as V

import           Disorder.Core.Property (failWith)
import           Disorder.Core.UniquePair (UniquePair(..))

import           P

import           System.IO (IO)

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()
import           Test.Warden.Arbitrary ()

import           Warden.Data.TextCounts

prop_hashText :: UniquePair ByteString -> Property
prop_hashText (UniquePair x y) =
  let x' = hashText x
      y' = hashText y in
  (x' /= y') === True

prop_combineUniqueTextCounts :: TextFreeformThreshold -> Property
prop_combineUniqueTextCounts fft =
  let n = (+) 1 $ (unTextFreeformThreshold fft) `div` 2
      l1 = take n [1..]
      l2 = take n [(unTextFreeformThreshold fft)..]
      a = UniqueTextCount $ S.fromList l1
      b = UniqueTextCount $ S.fromList l2 in
  (combineUniqueTextCounts fft a b) === LooksFreeform

prop_combineTextCounts :: TextFreeformThreshold -> TextCounts -> TextCounts -> Property
prop_combineTextCounts fft a b =
  case combineTextCounts fft a b of
    NoTextCounts -> (noCounts a && noCounts b) === True
    TextCounts csc ->
      case (a, b) of
        (TextCounts csa, TextCounts csb) ->
           ((V.all id $ V.zipWith gte csc csa)
             && (V.all id $ V.zipWith gte csc csb)) === True
        (TextCounts csa, NoTextCounts) ->
           (V.all id $ V.zipWith gte csc csa) === True
        (NoTextCounts, TextCounts csb) ->
           (V.all id $ V.zipWith gte csc csb) === True
        (NoTextCounts, NoTextCounts) ->
           failWith "non-null result combining two null TextCounts"
  where
    noCounts NoTextCounts = True
    noCounts (TextCounts _) = False

    gte LooksFreeform _ = True
    gte (UniqueTextCount _) LooksFreeform = False
    gte (UniqueTextCount csc') (UniqueTextCount csa') =
      S.size csc' >= S.size csa'

return []
tests :: IO Bool
tests = $quickCheckAll
